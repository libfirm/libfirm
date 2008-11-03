/*
 * Firm - Evaluator
 *
 * (C) 2005 Michael Beck    beck@ipd.info.uni-karlsruhe.de
 */
#include <windows.h>
#include <stdio.h>
#include <malloc.h>
#include <tchar.h>
#include "firm.h"
#include "firmEvaluator.h"

/**
 * Get the initial address. As the interface allows only 32 bit
 * transmitted, new I/F must be asked for 64bit support.
 */
static void *GetInitialAddress(DWORD dwAddress, DEBUGHELPER *pHelper) {
  if (pHelper->dwVersion < 0x20000) {
    /* VC 6.0 access */
    return (void *)dwAddress;
  }
  else {
    /* VC 7.0+ access */
    return (void *)pHelper->GetRealAddress(pHelper);
  }
}

/**
 * Copy from debuggee.
 */
HRESULT copy_from_debuggee(const void *address, DEBUGHELPER *pHelper, void *dst, DWORD size)
{
  DWORD nGot;

  if (pHelper->dwVersion < 0x20000) {
    /* VC 6.0 access */
    if (pHelper->ReadDebuggeeMemory(pHelper, (DWORD)address, size, dst, &nGot) != S_OK)
      return E_FAIL;
    if (nGot != size)
      return E_FAIL;
  }
  else {
    /* VC 7.0+ access */
    if (pHelper->ReadDebuggeeMemoryEx(pHelper, (DWORDLONG)address, size, dst, &nGot) != S_OK)
      return E_FAIL;
    if (nGot != size)
      return E_FAIL;
  }
  return S_OK;
}

/**
 * Evaluate an firm * object
 */
HRESULT WINAPI firm_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  firm_kind kind;
  int size;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == NULL) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  if (copy_from_debuggee(address, pHelper, &kind, sizeof(kind)) != S_OK)
    return E_FAIL;

  size = get_firm_object_size(kind);
  if (size <= 0)
    return E_FAIL;

  return FormatFirmObject(pHelper, nBase, kind, address, pResult, max);
}

/**
 * Evaluate an ident * object
 */
HRESULT WINAPI firm_ident_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == NULL) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_ident(pHelper, address, pResult, max);
}

/**
 * Evaluate a pset * object
 */
HRESULT WINAPI firm_pset_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == NULL) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_pset(pHelper, nBase, address, pResult, max);
}

/**
 * Evaluate a set * object
 */
HRESULT WINAPI firm_set_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == 0) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_set(pHelper, nBase, address, pResult, max);
}

/**
 * Evaluate a _arr_descr * object
 */
HRESULT WINAPI firm_arr_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == NULL) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_arr_descr(pHelper, nBase, address, pResult, max);
}

/**
 * Evaluate a pdeq * object
 */
HRESULT WINAPI firm_pdeq_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == 0) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_pdeq(pHelper, nBase, address, pResult, max);
}

/**
 * Evaluate a bitset_t * object
 */
HRESULT WINAPI firm_bitset_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == 0) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_bitset(pHelper, nBase, address, pResult, max);
}

/**
 * Evaluate a ir_op
 */
HRESULT WINAPI firm_op_Evaluate(DWORD dwAddress, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved )
{
  char *data = NULL;
  void *address = GetInitialAddress(dwAddress, pHelper);

  if (address == 0) {
    _tcsncpy(pResult, "NULL", max);
    return S_OK;
  }

  return format_op(pHelper, address, pResult, max);
}
