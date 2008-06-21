#include <Foundation/NSObject.h>

#ifndef AUTORELEASE

#define ASSIGN(VAR, VALUE) \
 ({ RELEASE (VAR); VAR = VALUE; RETAIN (VAR); })

#define ASSIGN_COPY(VAR, VALUE) \
 ({ RELEASE (VAR); VAR = [VALUE copy]; RETAIN (VAR); })

#define AUTORELEASE(VALUE) \
 [VALUE autorelease]

#define DESTROY(VAR) \
 ({ RELEASE (VAR); VAR = nil; })

#define RELEASE(VALUE) \
 [VALUE release]

#define RETAIN(VALUE) \
 [VALUE retain]

#endif // !AUTORELEASE
