/* -*- mode: objc; coding: utf-8 -*- */
/* Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
 * Copyright (C) 2008  Matthias Andreas Benkard.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "MLKReader.h"
#import "MLKCharacter.h"
#import "MLKReadtable.h"
#import "MLKEndOfFileError.h"
#import "MLKReaderError.h"
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKPackage.h"
#import "MLKClosure.h"
#import "MLKStream.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>


@implementation MLKReader
+(id) readFromStream:(MLKStream *)stream
            eofError:(BOOL)eofError
            eofValue:(id)eofValue
           recursive:(BOOL)recursive
  preserveWhitespace:(BOOL)preserveWhitespace
{
  unichar ch;
  NSMutableString *token;
  MLKReadtable *readtable;
  BOOL escaped;

  readtable = [[MLKDynamicContext currentContext]
                valueForBinding:[[MLKPackage findPackage:@"COMMON-LISP"]
                                  intern:@"*READTABLE*"]];
  
 start:
  if ([stream isEOF])
    {
      if (eofError)
        [[[MLKEndOfFileError alloc] initWithStream:stream] raise];
      else
        return eofValue;
    }

  ch = [stream readChar];
  if ([readtable isWhitespaceCharacter:ch])
    goto start;
  
  if ([readtable isMacroCharacter:ch])
    {
      NSArray *returnValues;
      MLKClosure *macrofun = [readtable macroFunctionForCharacter:ch];
      NSArray *args = [NSArray arrayWithObjects:
                                 stream,
                                 [MLKCharacter characterWithUnichar:ch],
                               nil];
      if ([args count] != 2)
        {
          args = [NSMutableArray arrayWithCapacity:2];
          [((NSMutableArray*)args) addObject:stream];
          [((NSMutableArray*)args) addObject:[MLKCharacter
                                               characterWithUnichar:ch]];
        }
      returnValues = [macrofun applyToArray:args];
      if ([returnValues count])
        return [returnValues objectAtIndex:0];
      else
        goto start;
    }
  
  escaped = NO;
  
  if ([readtable isSingleEscapeCharacter:ch])
    {
      if ([stream isEOF])
        [[[MLKEndOfFileError alloc] initWithStream:stream] raise];

      token = [NSMutableString stringWithCapacity:8];
      [token appendFormat:@"%C", [stream readChar]];
    }

  if ([readtable isMultipleEscapeCharacter:ch])
    {
      token = [NSMutableString stringWithCapacity:8];
      escaped = YES;
    }

  if ([readtable isConstituentCharacter:ch])
    {
      token = [NSMutableString stringWithCapacity:8];
      [token appendFormat:@"%C", [stream readChar]];
    }

  while (![stream isEOF])
    {
      ch = [stream readChar];
      if ([readtable isConstituentCharacter:ch] ||
          [readtable isNonTerminatingMacroCharacter:ch] ||
          (escaped && [readtable isWhitespaceCharacter:ch]))
        {
          if (escaped)
            [token appendFormat:@"%C", ch];
          else
            [token appendFormat:@"%C", [readtable charWithReadtableCase:ch]];
        }
      else if ([readtable isSingleEscapeCharacter:ch])
        {
          if ([stream isEOF])
            [[[MLKEndOfFileError alloc] initWithStream:stream] raise];
          
          token = [NSMutableString stringWithCapacity:8];
          [token appendFormat:@"%C", [stream readChar]];
        }
      else if ([readtable isMultipleEscapeCharacter:ch])
        escaped = !escaped;
      else if ([readtable isTerminatingMacroCharacter:ch])
        {
          [stream unreadChar:ch];
          break;
        }
      else if ([readtable isInvalid:ch])
        {
          [[[MLKReaderError alloc] initWithStream:stream] raise];
        }
      else if ([readtable isWhitespaceCharacter:ch])
        {
          if (preserveWhitespace)
            [stream unreadChar:ch];
          break;
        }
    }

  return [self interpretToken:token readtable:readtable];
}

+(BOOL) isPotentialNumber:(NSString *)token readtable:(MLKReadtable *)readtable
{
  // Check whether the token is a potential number.
  //
  // See CLHS 2.3.1.1.
  unsigned long i;
  unichar first;

  // 1. Does it consist solely of characters allowed in a potential
  // number?
  for (i = 0; i < [token length]; i++)
    {
      unichar ch = [token characterAtIndex:i];
      if (!([readtable isDigit:ch]
            || [readtable isSign:ch]
            || [readtable isRatioMarker:ch]
            || [readtable isDecimalPoint:ch]
            || ch == '^'
            || ch == '_'
            || ([readtable isNumberMarker:ch]
                // Adjacent number markers aren't to be considered number
                // markers at all.
                && (i == 0
                    || ![readtable
                          isNumberMarker:[token characterAtIndex:(i-1)]]))))
        return NO;
    }

  // 2. Does the token contain a digit?
  for (i = 0; i < [token length]; i++)
    {
      unichar ch = [token characterAtIndex:i];
      if ([readtable isDigit:ch])
        goto digitFound;
    }
  return NO;

 digitFound:
  // 3. Is the first character okay?
  first = [token characterAtIndex:0];
  if (!([readtable isDigit:first]
        || [readtable isSign:first]
        || [readtable isDecimalPoint:first]
        || first == '^'
        || first == '_'))
    return NO;

  // 4. Does the token not end with a sign?
  if ([readtable isSign:[token characterAtIndex:([token length]-1)]])
    return NO;

  return YES;
}

+(id) interpretToken:(NSString *)token readtable:(MLKReadtable *)readtable
{
  if ([self isPotentialNumber:token readtable:readtable])
    {
      // ???
    }
  else
    {
      unsigned long i, packageMarker;
      MLKPackage *package;
      NSString *symbolName;
      MLKSymbol *symbol;

      // Look for the package marker.
      packageMarker = -1;
      for (i = 0; i < [token length]; i++)
        {
          if ([readtable isPackageMarker:[token characterAtIndex:i]])
            {
              packageMarker = i;
              break;
            }
        }
      
      // Extract the package and symbol name.
      if (packageMarker == -1)
        {
          package = [[MLKDynamicContext currentContext]
                      valueForBinding:[[MLKPackage
                                         findPackage:@"COMMON-LISP"]
                                        intern:@"*PACKAGE*"]];
          symbolName = token;
        }
      else if (packageMarker == 0)
        {
          package = [MLKPackage findPackage:@"KEYWORD"];
          symbolName = [token substringFromIndex:1];
        }
      else
        {
          package = [MLKPackage
                      findPackage:[token substringToIndex:packageMarker]];
          if ([readtable isPackageMarker:[token characterAtIndex:(i+1)]])
            symbolName = [token substringFromIndex:(packageMarker+2)];
          else
            {
              // A single package marker means we have to check whether
              // the symbol is external in the package.
              symbolName = [token substringFromIndex:(packageMarker+1)];
              if (![[package exportedSymbols] containsObject:[package intern:token]])
                [[[MLKReaderError alloc] init] raise];
            }
        }

      symbol = [package intern:symbolName];
      
      if (packageMarker == 0)
        {
          // Make keyword symbols self-evaluate.
          [[MLKDynamicContext currentContext] setValue:symbol forBinding:symbol];
        }

      return symbol;
    }
}
@end
