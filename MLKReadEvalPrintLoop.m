/* -*- mode: objc; coding: utf-8 -*- */
/* Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
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

#import "MLKDynamicContext.h"
#import "MLKInterpreter.h"
#import "MLKLexicalEnvironment.h"
#import "MLKPackage.h"
#import "MLKReadEvalPrintLoop.h"
#import "MLKReader.h"
#import "NSObject-MLKPrinting.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSString.h>

#ifdef GNUSTEP
#import <Foundation/NSDebug.h>
#endif

#include <histedit.h>
#include <string.h>


static int _argc;
static char **_argv;


static const char *prompt (EditLine *e) {
  NS_DURING
    {
      MLKPackage *package = [[MLKDynamicContext currentContext]
                              valueForSymbol:[[MLKPackage
                                                findPackage:@"COMMON-LISP"]
                                               intern:@"*PACKAGE*"]];

      return [[NSString stringWithFormat:@"%@> ", [package name]] UTF8String];
    }
  NS_HANDLER
    {
      printf ("Caught an unhandled exception.\nName: %s\nReason: %s\n",
              [[localException name] UTF8String],
              [[localException reason] UTF8String]);
      return "> ";
    }
  NS_ENDHANDLER
}


@implementation MLKReadEvalPrintLoop : NSObject
+(void) run
{
  EditLine *editline;
  History *commands;
  HistEvent event;

  NSInputStream *input;
  MLKStream *stream;
  BOOL success;
  NSAutoreleasePool *pool;

  editline = el_init (_argv[0], stdin, stdout, stderr);
  el_set (editline, EL_PROMPT, &prompt);
  el_set (editline, EL_EDITOR, "emacs");

  commands = history_init();
  history (commands, &event, H_SETSIZE, 1000);
  el_set (editline, EL_HIST, history, commands);

  pool = [[NSAutoreleasePool alloc] init];

  printf ("Loading init.lisp.\n");
  NS_DURING
    {
      input = [NSInputStream inputStreamWithFileAtPath:@"init.lisp"];
      stream = LAUTORELEASE ([[MLKStream alloc] initWithInputStream:input]);

      [input open];
      [MLKInterpreter load:stream verbose:YES print:YES];
      success = [MLKInterpreter load:stream verbose:YES print:YES];
      [input close];
    }
  NS_HANDLER
    {
      printf ("Caught an unhandled exception.\nName: %s\nReason: %s\n",
              [[localException name] UTF8String],
              [[localException reason] UTF8String]);
    }
  NS_ENDHANDLER;

  printf ("Done.\n\n");

  printf ("This is Toilet Lisp, version 0.0.1.\n");
  printf ("Please make yourself at home.\n");

  [[MLKDynamicContext currentContext]
    setValue:[[MLKPackage findPackage:@"COMMON-LISP"]
               intern:@"T"]
    forSymbol:[[MLKPackage findPackage:@"TOILET-SYSTEM"]
                intern:@"*SYSTEM-INITIALISED-P*"]];

  LRELEASE (pool);

  while (1)
    {
      const char *line;
      int line_length;

      line = el_gets (editline, &line_length);

      if (line_length > 1)
        {
          NSArray *results;
          id code;

          pool = [[NSAutoreleasePool alloc] init];

          history (commands, &event, H_ENTER, line);

          if (strcmp (line, ":q\n") == 0 || strncmp (line, ":q ", 3) == 0)
            break;

#if 1
          NS_DURING
#else
          GSDebugAllocationActive (YES);
          [NSObject enableDoubleReleaseCheck:YES];
          NSZombieEnabled = YES;
#endif
            {
              int i;

              code = [MLKReader readFromString:[NSString stringWithUTF8String:line]];

              results = [MLKInterpreter eval:code
                                        inLexicalContext:[MLKLexicalContext
                                                           globalContext]
                                        withEnvironment:[MLKLexicalEnvironment
                                                          globalEnvironment]];

              for (i = 0; i < [results count]; i++)
                {
                  id result = [results objectAtIndex:i];
                  printf ("%s\n", [MLKPrintToString (denullify (result)) UTF8String]);
                }
            }
#if 1
          NS_HANDLER
            {
              printf ("Caught an unhandled exception.\nName: %s\nReason: %s\n",
                      [[localException name] UTF8String],
                      [[localException reason] UTF8String]);
            }
          NS_ENDHANDLER;
#endif

          LRELEASE (pool);
        }

      //free (line);
    }

  history_end (commands);
  el_end (editline);
}
@end


int main (int argc, char **argv)
{
  _argc = argc;
  _argv = argv;
  NSAutoreleasePool *pool;
  pool = [[NSAutoreleasePool alloc] init];
  [MLKReadEvalPrintLoop run];
  LRELEASE (pool);
  return 0;
}
