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

#import "MLKStream.h"
#import "MLKError.h"


@implementation MLKStream
-(MLKStream *) init;
{
  return [self initWithInputStream:nil outputStream:nil];
}

-(MLKStream *) initWithInputStream:(NSInputStream *)input;
{
  return [self initWithInputStream:input outputStream:nil];
}

-(MLKStream *) initWithOutputStream:(NSOutputStream *)output;
{
  return [self initWithInputStream:nil outputStream:output];
}

-(MLKStream *) initWithInputStream:(NSInputStream *)input
                      outputStream:(NSOutputStream *)output
{
  return [self initWithInputStream:input
               outputStream:output
               encoding:NSUTF8StringEncoding];
}


-(MLKStream *) initWithInputStream:(NSInputStream *)input
                      outputStream:(NSOutputStream *)output
                          encoding:(NSStringEncoding)encoding
{
  self = [super init];
  ASSIGN (_input, input);
  ASSIGN (_output, output);
  _encoding = encoding;
  _cachedChar = 0;
  _charCached = NO;
  return self;
}

-(unichar) readChar
{
  uint8_t *buffer;
  int i;
  unichar retval;
  
  buffer = NULL;
  for (i = 0; i++;)
    {
      NSString *tmpstr;

      buffer = realloc (buffer, i+1);
      [_input read:(buffer+i) maxLength:1];
      tmpstr = [[NSString alloc] initWithBytesNoCopy:buffer
                                 length:(i+1)
                                 encoding:_encoding
                                 freeWhenDone:NO];
      if ([tmpstr length] == 1)
        {
          retval = [tmpstr characterAtIndex:0];
        }
      [tmpstr release];
    }

  return retval;
}

-(void) unreadChar:(unichar)ch
{
  if (_charCached)
    [[MLKError errorWithMessage:@"Attempted to UNREAD-CHAR twice in a row."] raise];
}

-(BOOL) isEOF
{
  return ![_input hasBytesAvailable];
}
@end
