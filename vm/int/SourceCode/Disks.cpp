//////////////////////////////////////////////////////////////////////////////
// Disks.cpp  Kronos disks subsystem
//
// Author:
//      Dmitry ("Leo") Kuznetsov        LeoK@myself.com
// Revision History
//      Jan 20, 1998 - originated       

#include "preCompiled.h"
#include "DISKS.h"

DISKS::DISKS()
{
    for (int i = 0; i < N; i++)
        fDisks[i] = INVALID_HANDLE_VALUE;
    memset(fName, 0, sizeof fName);
    memset(nMount, 0, sizeof fName);
    memset(bFloppy, 0, sizeof bFloppy);
    nDiskCount = 0;
}


DISKS::~DISKS()
{
    for (int i = 0; i < nDiskCount; ++i)
    {
        GlobalFreePtr(fName[i]);
        fName[i] = NULL;
        nMount[i] = 0;
        Dismount(i);
    }
}

    
bool DISKS::AddDisk(const char* szFileName)
{
    if (nDiskCount >= N)
        return false;
    fName[nDiskCount] = (char*)GlobalAllocPtr(GPTR, strlen(szFileName) + 1);
    if (fName[nDiskCount] == INVALID_HANDLE_VALUE || fName[nDiskCount] == NULL)
        return false;
    strcpy(fName[nDiskCount], szFileName);

    nDiskCount++;
    return true;
}


int DISKS::GetCount()
{
    return nDiskCount;
}


bool DISKS::Mount(int n)
{
//  trace("DISKS::Mount(%d)\n", n);
    if (n < 0 || n >= nDiskCount)
        return false;

    if (fDisks[n] != INVALID_HANDLE_VALUE)
    {
        nMount[n]++;
        return true;
    }
    bFloppy[n] = strcmp(fName[n], "\\\\.\\A:") == 0 || strcmp(fName[n], "\\\\.\\a:") == 0;
    if (bFloppy[n])
        fDisks[n] = CreateFile("\\\\.\\A:", GENERIC_READ|GENERIC_WRITE, 
                            FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE, NULL, 
                            OPEN_EXISTING, 
                            FILE_FLAG_RANDOM_ACCESS|FILE_FLAG_WRITE_THROUGH|FILE_FLAG_NO_BUFFERING,
                            NULL);
    else
        fDisks[n] = CreateFile( fName[n], 
                                GENERIC_READ|GENERIC_WRITE, 
                                0, // FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE
                                NULL, OPEN_EXISTING, 
                                FILE_FLAG_RANDOM_ACCESS, NULL);
    if (fDisks[n] != INVALID_HANDLE_VALUE)
    {
        nMount[n]++;
        return true;
    }
    return false;
}


bool DISKS::Dismount(int n)
{
//  trace("DISKS::Dismount(%d)\n", n);
    if (fDisks[n] == INVALID_HANDLE_VALUE)
        return false;
    if (nMount[n] > 0)
        nMount[n]--;
    if (nMount[n] > 0)
        return true;
    CloseHandle(fDisks[n]);
    fDisks[n] = INVALID_HANDLE_VALUE;
    return true;
}


bool DISKS::Read(int n, int sectorno, byte* adr, int len)
{
//  trace("read(%d, %d, %08X, %d)\n", n, sectorno, adr, len);
    bool bMount = false;
    if (fDisks[n] == INVALID_HANDLE_VALUE)
    {
        if (!Mount(n))
            return false;
        bMount = true;
    }
    dword nRead = 0;
    if ((int)SetFilePointer(fDisks[n], sectorno*512, NULL, FILE_BEGIN) >= 0)
    {
        if (!ReadFile(fDisks[n], adr, len, &nRead, NULL))
            nRead = 0;
    }
    if (bMount)
        Dismount(n);
    return (int)nRead == len;
}


bool DISKS::Write(int n, int sectorno, byte* adr, int len)
{
//  trace("write(%d, %d, %08X, %d)\n", n, sectorno, adr, len);
    bool bMount = false;
    if (fDisks[n] == INVALID_HANDLE_VALUE)
    {
        if (!Mount(n))
            return false;
        bMount = true;
    }
    dword nWritten = 0;
    if ((int)SetFilePointer(fDisks[n], sectorno*512, NULL, FILE_BEGIN) >= 0)
    {
        if (!WriteFile(fDisks[n], adr, len, &nWritten, NULL))
            nWritten = 0;
    }
    if (bMount)
        Dismount(n);
    return len == (int)nWritten;
}


int DISKS::GetFloppySize4KB(int n, dword &SectorsPerCluster, 
                            dword &BytesPerSector, dword &TotalNumberOfClusters)
{
    // unless we close the "\\.\A:" file and iterate   
    // direcotry on the floppy disk
    // GetDiskFreeSpace() will return -1 if this is
    // first access. This will make Excelsior to think
    // there is no floppy in the bay
    bool bNeedToMount = false;
    if (fDisks[n] != INVALID_HANDLE_VALUE)
    {
        Dismount(n);
        bNeedToMount = true;
    }
    WIN32_FIND_DATA findFileData;
    HANDLE hNiceTry = FindFirstFile("A:\\*.*", &findFileData);
    if (hNiceTry != INVALID_HANDLE_VALUE)
    {
        ::FindClose(hNiceTry);  hNiceTry = INVALID_HANDLE_VALUE;
    }

    int size = -1;
    DWORD NumberOfFreeClusters = 0;
    if (::GetDiskFreeSpace("A:\\", 
        &SectorsPerCluster,
        &BytesPerSector,
        &NumberOfFreeClusters,
        &TotalNumberOfClusters))
    {
        size = TotalNumberOfClusters * SectorsPerCluster * BytesPerSector;
        size = (int)(size / (4*K));
    }
    else
        size = -1;
    if (bNeedToMount)
        Mount(n);
    return size;
}


bool DISKS::GetSize4KB(int n, int* adr)
{
    bool bMount = false;
    if (fDisks[n] == INVALID_HANDLE_VALUE)
    {
        if (!Mount(n))
            return false;
        bMount = true;
    }
    dword dwSizeLo = 0;
    if (!bFloppy[n])
    {
        dwSizeLo = GetFileSize(fDisks[n], NULL);
        if (dwSizeLo != 0xFFFFFFFF)
            *adr = (int)(dwSizeLo / (4*K));
    }
    else
    {
        dword SectorsPerCluster = 0;
        dword BytesPerSector = 0;
        dword TotalNumberOfClusters = 0;
        dwSizeLo = GetFloppySize4KB(n, SectorsPerCluster, BytesPerSector, TotalNumberOfClusters);
        if (dwSizeLo != 0xFFFFFFFF)
            *adr = dwSizeLo;
    }
    if (bMount)
        Dismount(n);
    return dwSizeLo != 0xFFFFFFFF;
}


bool DISKS::GetSpecs(int n, Request* pRequest)
{
    if (!bFloppy[n])
        return false;
    dword SectorsPerCluster = 0;
    dword BytesPerSector = 0;
    dword TotalNumberOfClusters = 0;
    GetFloppySize4KB(n, SectorsPerCluster, BytesPerSector, TotalNumberOfClusters);
    dword TotalNumberOfSectors = TotalNumberOfClusters * SectorsPerCluster;
    dword TotalNumberOfBytes = TotalNumberOfSectors * BytesPerSector;
    pRequest->res = 0;
    pRequest->dmode = floppy | ready;
    pRequest->dsecs = TotalNumberOfSectors;
    pRequest->ssc = 0;
    int j = BytesPerSector;
    while (j > 1)
    {
        j = j / 2;
        pRequest->ssc++;
    }
    pRequest->secsize = BytesPerSector;
    pRequest->cyls = TotalNumberOfBytes <= 720*1024 ? 40 : 80;
    pRequest->heads = 2;
    pRequest->minsec = 1;
    pRequest->maxsec = 1 + TotalNumberOfSectors / (pRequest->cyls * pRequest->heads);
    pRequest->ressec = 0;
    pRequest->precomp = 0;
    pRequest->rate = 0;
    dword dwSize = (pRequest->maxsec - pRequest->minsec + 1) * BytesPerSector 
                  * pRequest->cyls * pRequest->heads;
    unused(dwSize);
//  trace("A: size = %d %dKB\n", dwSize, dwSize / 1024);
//  assert((pRequest->maxsec - pRequest->minsec + 1) * BytesPerSector
//          * pRequest->cyls * pRequest->heads == TotalNumberOfBytes);
    return true;
}


bool DISKS::SetSpecs(int n, Request* pRequest)
{
    (void)pRequest;
    if (!bFloppy[n])
        return true;
    return false;
}
