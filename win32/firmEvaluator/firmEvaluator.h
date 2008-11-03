/*
 * Firm - Evaluator
 *
 * (C) 2005 Michael Beck    beck@ipd.info.uni-karlsruhe.de
 */
#ifndef __FIRM_EVALUATOR_H__
#define __FIRM_EVALUATOR_H__

typedef struct tagDEBUGHELPER {
  DWORD dwVersion;
  BOOL (WINAPI *ReadDebuggeeMemory)( struct tagDEBUGHELPER *pThis, DWORD dwAddr, DWORD nWant, VOID* pWhere, DWORD *nGot );
  /* from here only when dwVersion >= 0x20000 */
  DWORDLONG (WINAPI *GetRealAddress)( struct tagDEBUGHELPER *pThis );
  BOOL (WINAPI *ReadDebuggeeMemoryEx)( struct tagDEBUGHELPER *pThis, DWORDLONG qwAddr, DWORD nWant, VOID* pWhere, DWORD *nGot );
  int (WINAPI *GetProcessorType)( struct tagDEBUGHELPER *pThis );
} DEBUGHELPER;

typedef HRESULT (WINAPI *CUSTOMVIEWER)( void *address, DEBUGHELPER *pHelper, int nBase, BOOL bUniStrings, char *pResult, size_t max, DWORD reserved );

/**
 * Copy from debuggee
 *
 * @param address  address in debuggee's space to copy from
 * @param pHelper  debugger helper
 * @param dst      pointer in our space to copy to
 * @param size     number of bytes to copy
 */
HRESULT copy_from_debuggee(const void *address, DEBUGHELPER *pHelper, void *dst, DWORD size);

/**
 * return the size of a firm object
 *
 * @kind  the firm kind
 */
int get_firm_object_size(firm_kind kind);

/**
 * Format an ident
 *
 * @param pHelper  debugger helper
 * @param address  ident address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_ident(DEBUGHELPER *pHelper, const void *address, char *pResult, size_t max);

/**
 * Format an ir_op
 *
 * @param pHelper  debugger helper
 * @param address  ident address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_op(DEBUGHELPER *pHelper, const void *address, char *pResult, size_t max);

/**
 * Format a pset
 *
 * @param pHelper  debugger helper
 * @param nBase    base for number's
 * @param address  pset address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_pset(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max);

/**
 * Format a set
 *
 * @param pHelper  debugger helper
 * @param nBase    base for number's
 * @param address  set address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_set(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max);

/**
 * Format an array descriptor
 *
 * @param pHelper  debugger helper
 * @param nBase    base for number's
 * @param address  array descriptor address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_arr_descr(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max);

/**
 * Format a pointer double ended queue
 *
 * @param pHelper  debugger helper
 * @param nBase    base for number's
 * @param address  pdeq address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_pdeq(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max);

/*
 * Format a bitset
 *
 * @param pHelper  debugger helper
 * @param nBase    base for number's
 * @param address  bitset address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT format_bitset(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max);

/**
 * format a firm object
 *
 * @param pHelper  debugger helper
 * @param nBase    base for number's
 * @param kind     Firm kind
 * @param address  firm object address in debuggee's space
 * @param pResult  string buffer
 * @param max      length of the string buffer
 */
HRESULT FormatFirmObject(DEBUGHELPER *pHelper, int nBase, firm_kind kind, const void *address, char *pResult, size_t max);

#endif /* __FIRM_EVALUATOR_H__ */
