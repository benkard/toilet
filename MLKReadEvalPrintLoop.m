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

#import "MLKDynamicContext.h"
#import "MLKInterpreter.h"
#import "MLKLexicalEnvironment.h"
#import "MLKPackage.h"
#import "MLKReadEvalPrintLoop.h"
#import "MLKReader.h"
#import "NSObject-MLKPrinting.h"
#import "runtime-compatibility.h"

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSException.h>
#import <Foundation/NSString.h>

#import <editline/history.h>
#import <editline/readline.h>
#import <histedit.h>


static int _argc;
static char **_argv;


static const char *prompt (EditLine *e) {
  MLKPackage *package = [[MLKDynamicContext currentContext]
                          valueForSymbol:[[MLKPackage
                                             findPackage:@"COMMON-LISP"]
                                            intern:@"*PACKAGE*"]];
  
  return [[NSString stringWithFormat:@"%@> ", [package name]] UTF8String];
}


@implementation MLKReadEvalPrintLoop : NSObject
+(void) run
{
  EditLine *editline;
  History *commands;
  HistEvent event;

  editline = el_init (_argv[0], stdin, stdout, stderr);
  el_set (editline, EL_PROMPT, &prompt);
  el_set (editline, EL_EDITOR, "emacs");

  commands = history_init();
  history (commands, &event, H_SETSIZE, 1000);
  el_set (editline, EL_HIST, history, commands);
  
  printf ("This is Toilet Lisp, version 0.0.1.\n");
  printf ("Please make yourself at home.\n");

  while (1)
    {
      const char *line;
      ssize_t line_length;

      line = el_gets (editline, &line_length);

      if (line_length > 1)
        {
          NSAutoreleasePool *pool;
          id result;
          id code;

          pool = [[NSAutoreleasePool alloc] init];

          history (commands, &event, H_ENTER, line);

          NS_DURING
            {
              code = [MLKReader readFromString:[NSString stringWithUTF8String:line]];

              result = [MLKInterpreter eval:code
                                       inLexicalContext:[MLKLexicalContext
                                                          globalContext]
                                       withEnvironment:[MLKLexicalEnvironment
                                                         globalEnvironment]];
              
              if (result)
                printf ("%s\n", [[result descriptionForLisp] UTF8String]);
              else
                printf ("()\n");
            }
          NS_HANDLER
            {
              printf ("Caught an unhandled exception.\nName: %s\nReason: %s\n",
                      [[localException name] UTF8String],
                      [[localException reason] UTF8String]);
            }
          NS_ENDHANDLER

          RELEASE (pool);
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
  RELEASE (pool);
  return 0;
}
