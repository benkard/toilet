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
#import "MLKReaderError.h"
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKPackage.h"
#import "MLKFuncallable.h"
#import "MLKStream.h"
#import "MLKFloat.h"
#import "MLKInteger.h"
#import "MLKRatio.h"
#import "MLKStringInputStream.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSRange.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>


@implementation MLKReader
+(id) readFromStream:(MLKStream *)stream
            eofError:(BOOL)eofError
            eofValue:(id)eofValue
           recursive:(BOOL)recursive
  preserveWhitespace:(BOOL)preserveWhitespace
{
  return [self readFromStream:stream
               eofError:eofError
               eofValue:eofValue
               recursive:recursive
               preserveWhitespace:preserveWhitespace
               singleDotMarker:nil];
}

+(id) readFromStream:(MLKStream *)stream
            eofError:(BOOL)eofError
            eofValue:(id)eofValue
           recursive:(BOOL)recursive
  preserveWhitespace:(BOOL)preserveWhitespace
     singleDotMarker:(id)dotMarker
{
  unichar ch;
  NSMutableString *token;
  MLKReadtable *readtable;
  BOOL escaped;
  BOOL ever_escaped;

  ever_escaped = NO;

  readtable = [[MLKDynamicContext currentContext]
                valueForSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                                  intern:@"*READTABLE*"]];

 start:
  if ([stream isEOF])
    {
      if (eofError)
        [NSException raise:@"MLKEndOfFileError"
                     format:@"Premature end of file on stream %@.", stream];
      else
        return eofValue;
    }

  ch = [stream readChar];
  if ([readtable isWhitespaceCharacter:ch] || ch == '\0')
    goto start;

  if ([readtable isMacroCharacter:ch])
    {
      NSArray *returnValues;
      MLKFuncallable *macrofun = [readtable macroFunctionForCharacter:ch];
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
        return denullify ([returnValues objectAtIndex:0]);
      else
        goto start;
    }
  
  escaped = NO;
  
  if ([readtable isSingleEscapeCharacter:ch])
    {
      if ([stream isEOF])
        [NSException raise:@"MLKEndOfFileError"
                     format:@"Premature end of file on stream %@.", stream];;

      token = [NSMutableString stringWithCapacity:8];
      [token appendFormat:@"%C", [stream readChar]];
      ever_escaped = YES;
    }

  if ([readtable isMultipleEscapeCharacter:ch])
    {
      token = [NSMutableString stringWithCapacity:8];
      escaped = YES;
      ever_escaped = YES;
    }

  if ([readtable isConstituentCharacter:ch])
    {
      //NSLog (@"--> Constituent (%C)", ch);
      token = [NSMutableString stringWithCapacity:8];
      [token appendFormat:@"%C", [readtable charWithReadtableCase:ch]];
    }

  while (![stream isEOF])
    {
      //NSLog (@"...");
      ch = [stream readChar];
      if ([readtable isConstituentCharacter:ch] ||
          [readtable isNonTerminatingMacroCharacter:ch] ||
          (escaped && (![readtable isMultipleEscapeCharacter:ch]
                       && ![readtable isSingleEscapeCharacter:ch])))
        {
          if (escaped)
            [token appendFormat:@"%C", ch];
          else
            [token appendFormat:@"%C", [readtable charWithReadtableCase:ch]];
        }
      else if ([readtable isSingleEscapeCharacter:ch])
        {
          if ([stream isEOF])
            [NSException raise:@"MLKEndOfFileError"
                         format:@"Premature end of file on stream %@.", stream];
          
          [token appendFormat:@"%C", [stream readChar]];
          ever_escaped = YES;
        }
      else if ([readtable isMultipleEscapeCharacter:ch])
        {
          ever_escaped = YES;
          escaped = !escaped;
        }
      else if ([readtable isTerminatingMacroCharacter:ch])
        {
          [stream unreadChar:ch];
          break;
        }
      else if ([readtable isConstituentCharacter:ch]
               && [readtable isInvalid:ch])
        {
          //[[[MLKReaderError alloc] initWithStream:stream] raise];
          [NSException raise:@"MLKReaderError"
                       format:@"'%c' is an invalid constituent character.", ch];
        }
      else if ([readtable isWhitespaceCharacter:ch])
        {
          if (preserveWhitespace)
            [stream unreadChar:ch];
          break;
        }
    }

  //NSLog (@"--> Interpret token: %@", token);

  if ([token isEqualToString:@"."])
    {
      if (dotMarker)
        return dotMarker;
      else
        [NSException raise:@"MLKReaderError"
                     format:@"Unexpectedly read a single dot."];
    }

  return [self interpretToken:token
               readtable:readtable
               escaped:ever_escaped];
}

+(BOOL) isPotentialNumber:(NSString *)token
                readtable:(MLKReadtable *)readtable
                     base:(int)base
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
      if (!([readtable isDigit:ch inBase:base]
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
      if ([readtable isDigit:ch inBase:base])
        goto digitFound;
    }
  return NO;

 digitFound:
  // 3. Is the first character okay?
  first = [token characterAtIndex:0];
  if (!([readtable isDigit:first inBase:base]
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

+(id) interpretToken:(NSString *)token
           readtable:(MLKReadtable *)readtable
             escaped:(BOOL)escaped
{
  int base;
  
  base = [[[MLKDynamicContext currentContext]
            valueForSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                              intern:@"*READ-BASE*"]]
           intValue];

  if (!escaped && [self isPotentialNumber:token readtable:readtable base:base])
    {
      unsigned long i, firstNum, secondNum, exponent, exponentMarkerPos;
      unichar sign, exponentSign;
      unichar firstSeparator, exponentMarker;
      BOOL negative;


      // Read the sign (if present).
      if ([readtable isSign:[token characterAtIndex:0]])
        {
          sign = [token characterAtIndex:0];
          i = 1;
          firstNum = 1;
        }
      else
        {
          i = 0;
          firstNum = 0;
        }

      negative = (firstNum > 0 && [readtable isMinusSign:sign]);
 
      while ((i < [token length])
             && [readtable isDecimalDigit:[token characterAtIndex:i]])
        i++;

      if (i == [token length])
        {
          //NSLog (@"...");
          return [MLKInteger integerWithString:
                               [token substringWithRange:
                                        NSMakeRange (firstNum, [token length] - firstNum)]
                             negative:negative
                             base:10];
        }

      firstSeparator = [token characterAtIndex:i];

      if (!([readtable isDecimalPoint:firstSeparator]
            || [readtable isExponentMarker:[token characterAtIndex:i]]))
        goto digits;

      i++;
      secondNum = i;

      if (i == [token length] && [readtable isDecimalPoint:firstSeparator])
        {
          //NSLog (@"+++");
          return [MLKInteger integerWithString:
                               [token substringWithRange:
                                        NSMakeRange (firstNum, [token length] - firstNum - 1)]
                             negative:negative
                             base:10];
        }
      else
        {
          // We're dealing with a floating point number.  Bah.  I hate
          // floating point numbers.
          if ([readtable isExponentMarker:firstSeparator])
            {
              exponentMarkerPos = i;
              if ([readtable isSign:[token characterAtIndex:i]])
                {
                  exponentSign = [token characterAtIndex:i];
                  i++;
                }
              else
                exponentSign = '+';

              exponent = i;

              while ((i < [token length])
                     && [readtable isDecimalDigit:[token characterAtIndex:i]])
                i++;

              //NSLog (@"...2");
              //NSLog (@"%@, %@",
              //       [token substringWithRange:NSMakeRange (firstNum, exponentMarkerPos - firstNum - 1)],
              //       [token substringFromIndex:exponent]);
              return [MLKFloat floatWithExponentMarker:firstSeparator
                               integerPart:[token substringWithRange:NSMakeRange(firstNum, exponentMarkerPos - firstNum - 1)]
                               negative:negative
                               fractionalPart:@""
                               exponent:[token substringFromIndex:exponent]
                               exponentNegative:(exponentSign == '-')];
            }
          else
            {
              while ((i < [token length])
                     && [readtable isDecimalDigit:[token characterAtIndex:i]])
                i++;
              
              if (i == [token length])
                {
                  //NSLog (@"...3");
                  //NSLog (@"%@, %@",
                  //       [token substringWithRange:NSMakeRange (firstNum, secondNum - firstNum - 1)],
                  //       [token substringFromIndex:secondNum]);
                  return [MLKFloat floatWithExponentMarker:firstSeparator
                                   integerPart:[token substringWithRange:NSMakeRange (firstNum, secondNum - firstNum - 1)]
                                   negative:negative
                                   fractionalPart:[token substringFromIndex:secondNum]
                                   exponent:@""
                                   exponentNegative:NO];
                }

              // Assume token[i] is an exponent marker.
              exponentMarkerPos = i;
              exponentMarker = [token characterAtIndex:i];
              i++;

              if ([readtable isSign:[token characterAtIndex:i]])
                {
                  exponentSign = [token characterAtIndex:i];
                  i++;
                }
              else
                exponentSign = '+';

              exponent = i;

              while ((i < [token length])
                     && [readtable isDecimalDigit:[token characterAtIndex:i]])
                i++;

              //NSLog (@"...4");
              //NSLog (@"%@, %@, %@",
              //       [token substringWithRange:NSMakeRange (firstNum, secondNum - firstNum - 1)],
              //       [token substringWithRange:NSMakeRange (secondNum, exponentMarkerPos - secondNum)],
              //       [token substringFromIndex:exponent]);
              return [MLKFloat floatWithExponentMarker:exponentMarker
                               integerPart:[token substringWithRange:NSMakeRange (firstNum, secondNum - firstNum - 1)]
                               negative:negative
                               fractionalPart:[token substringWithRange:NSMakeRange (secondNum, exponentMarkerPos - secondNum)]
                               exponent:[token substringFromIndex:exponent]
                               exponentNegative:(exponentSign == '-')];              
            }
        }

    digits:
      i = firstNum;
      while ((i < [token length])
             && [readtable isDigit:[token characterAtIndex:i] inBase:base])
        i++;

      if (i == [token length])
        {
          //NSLog (@"###");
          return [MLKInteger integerWithString:
                               [token substringWithRange:
                                        NSMakeRange (firstNum, [token length] - firstNum)]
                             negative:negative
                             base:base];
        }

      // Assume token[i] is a slash.
      i++;
      secondNum = i;

      //NSLog (@"RRR");
      //NSLog (@"n: %@", [token substringWithRange:
      //                          NSMakeRange (firstNum,
      //                                       secondNum - firstNum - 1)]);
      //NSLog (@"d: %@", [token substringFromIndex:secondNum]);
      return [MLKRatio ratioWithNumeratorString:
                         [token substringWithRange:
                                  NSMakeRange (firstNum,
                                               secondNum - firstNum - 1)]
                       denominatorString:[token substringFromIndex:secondNum]
                       negative:negative
                       base:base];
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
                      valueForSymbol:[[MLKPackage
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
          NSString *packageName = [token substringToIndex:packageMarker];

          if ([packageName isEqualToString:@"#"])
            package = nil;
          else
            {
              package = [MLKPackage findPackage:packageName];

              if (!package)
                [NSException raise:@"MLKReaderError"
                             format:@"Can't find package %@.",
                             [token substringToIndex:packageMarker]];
            }

          if ([readtable isPackageMarker:[token characterAtIndex:(i+1)]])
            symbolName = [token substringFromIndex:(packageMarker+2)];
          else
            {
              // A single package marker means we have to check whether
              // the symbol is external in the package.
              symbolName = [token substringFromIndex:(packageMarker+1)];

              if (package)
                {
                  symbol = [package intern:symbolName];
                  if (![[package exportedSymbols] containsObject:symbol])
                    [NSException raise:@"MLKReaderError"
                                 format:@"Package %@ does not export symbol %@.",
                                        [package name],
                                        [symbol descriptionForLisp]];
                }
            }
        }

      if (package)
        symbol = [package intern:symbolName];
      else
        symbol = [MLKSymbol symbolWithName:symbolName package:nil];

      if (packageMarker == 0)
        {
          // Make keyword symbols self-evaluate.
          [[MLKDynamicContext globalContext] addValue:symbol forSymbol:symbol];
        }

      return symbol;
    }
}

+(id) readFromString:(NSString *)string
{
  return [self readFromStream:[[MLKStringInputStream alloc]
                                initWithString:string]
               eofError:YES
               eofValue:nil
               recursive:NO
               preserveWhitespace:NO];
}
@end
