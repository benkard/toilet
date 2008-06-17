/**
    STShell
    StepTalk Shell
 
    Copyright (c) 2002 Free Software Foundation
 
    Written by: Stefan Urbanek <urbanek@host.sk>
    Date: 2002 May 29
   
    This file is part of the StepTalk project.
 
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
 
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02111, USA.
 
 */

#import "STShell.h"

#import <StepTalk/StepTalk.h>

#import <Foundation/NSArray.h>
#import <Foundation/NSBundle.h>
#import <Foundation/NSDebug.h>
#import <Foundation/NSNotification.h>
#import <Foundation/NSException.h>
#import <Foundation/NSFileManager.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>
#import <Foundation/NSValue.h>

#include <readline/readline.h>

static Class NSString_class;
static Class NSNumber_class;

NSArray *objcSelectors = nil;

static STShell	*sharedShell = nil;

@interface STShell(STPrivateMethods)
- (int) completion;
- (NSString *)readLine;
- (void)initReadline;
@end

int complete_handler(void)
{
    return [sharedShell completion];
}

@implementation STShell

+ (void)initialize
{
    NSString_class = [NSString class];
    NSNumber_class = [NSNumber class];
}

+ sharedShell
{
    return sharedShell;
}

- initWithConversation:(STConversation *)conv
{
    self = [super init];
    
    [self initReadline];

    objectStack = [[NSMutableArray alloc] init];
    
    [[NSNotificationCenter defaultCenter]
                              addObserver:self
                                 selector:@selector(bundleLoaded:)
                                     name:NSBundleDidLoadNotification
                                   object:nil];
   
    scriptsManager = RETAIN([STScriptsManager defaultManager]);
    prompt = @"StepTalk > ";
    
    conversation = RETAIN(conv);
    
    /* FIXME: make this more clever for completion handler */
    if(!sharedShell)
    {
        sharedShell = self;
    }

    return self;
}

- (void)updateCompletionList
{
    NSMutableArray *array = [[NSMutableArray alloc] init];
    RELEASE(completionList);
    
    [array addObjectsFromArray:STAllObjectiveCSelectors()];

    completionList = [[NSArray alloc] initWithArray:array];

    updateCompletionList = NO;
}

- (void)dealloc
{
    RELEASE(objectStack);
    RELEASE(completionList);
    RELEASE(scriptsManager);
    RELEASE(conversation);
    
    [[NSNotificationCenter defaultCenter] removeObserver:self];

    [super dealloc];
}
-(void)bundleLoaded:(NSNotification *)notif
{
    updateCompletionList = YES;
}

- (void)initReadline
{
    rl_initialize();
    rl_bind_key('\t', complete_handler);
}

- (void)setLanguage:(NSString *)langName
{
    NSDebugLog(@"Setting language to %@", langName);

    [conversation setLanguage:langName];
}

- (void)setEnvironment:(STEnvironment *)newEnv
{
    [conversation setEnvironment:newEnv];
}

- (STEnvironment *)environment
{
    return [conversation context];
}

- (void)run
{
    STEnvironment *env;
    NSString      *line;
    id             result;
            
    [self showLine:@"Welcome to the StepTalk shell."];
    
    // NSLog(@"Environment %@", env);

    if(![conversation isKindOfClass:[STRemoteConversation class]])
    {
        completionEnabled = YES;
    }
    else
    {
        [self showLine:@"Note: Completion disabled for distant conversation"];
    }    
    
    while(1)
    {
        line = [self readLine];

        if(exitRequest)
            break;

        if(!line)
            continue;

        result = [self executeLine:line];

        if(result)
        {
            if(result != objectStack)
            {
                [objectStack addObject:result];
            }
            [self showResult:result];
        }
        else
        {
            [self showResult:result];
        }
        
    }
    printf("\n");
}
- (id)executeLine:(NSString *)line
{
    NSString      *cmd;
    id             result = nil;

    /* FIXME: why? */

    cmd = [line stringByAppendingString:@" "];
    NS_DURING
        [conversation interpretScript:cmd];
        result = [conversation result];
    NS_HANDLER
        [self showException:localException];
    NS_ENDHANDLER

    return result;
}

- (NSString *)readLine
{
    char       *str;
    NSString   *actualPrompt = prompt;
    NSString   *line = @"";
    BOOL        done = NO;
    int         len;
    
    while(!done)
    {
        str = readline([actualPrompt cString]);
        done = YES;

        if(!str)
        {
            exitRequest = YES;
            return nil;
        }

        len = strlen(str);
        if(!len)
            return nil;
        
        if(str[len-1] == '\\')
        {
            actualPrompt = @"... ? ";
            str[strlen(str) - 1] = '\0';
            done = NO;
        }
         
        line = [line stringByAppendingString:[NSString stringWithCString:str]];
    }

    add_history([line cString]);
    
    return line;
}

- (int)completion
{
    STEnvironment *env;
    NSEnumerator  *enumerator;
    NSMutableSet  *set;
    NSString      *match;
    NSString      *tail;
    NSString      *str;
    NSArray       *array;
    int pos = 0;
    int c;
    
    if(!completionEnabled)
    {
        return 0;
    }
    
    if(rl_point <= 0)
    {
        return 0;
    }
    
    pos = rl_point - 1;
    c = rl_line_buffer[pos];
    
    while((isalnum(c) || c == '_') && pos >= 0)
    {
        pos--;
        c = rl_line_buffer[pos];
    }
    pos++;

    match = [NSString stringWithCString:rl_line_buffer + pos
                                 length:rl_point - pos];

    set = [NSMutableSet set];
    
    if(!completionList || updateCompletionList)
    {
        [self updateCompletionList];
    }
    
    enumerator = [completionList objectEnumerator];
    while( (str = [enumerator nextObject]) )
    {
        if( [str hasPrefix:match] )
        {
            [set addObject:str];
        }
    }

    env = [conversation context];
    enumerator = [[env knownObjectNames] objectEnumerator];
    while( (str = [enumerator nextObject]) )
    {
        if( [str hasPrefix:match] )
        {
            [set addObject:str];
        }
    }

    array = [set allObjects];

    if( [array count] == 0 )
    {
        printf("\nNo match for completion.\n");
        rl_forced_update_display();
    }
    else if ( [array count] == 1 )
    {
        str = [array objectAtIndex:0];
        str = [str substringFromIndex:rl_point - pos];
        rl_insert_text([str cString]);
        rl_insert_text(" ");

        rl_redisplay();
    }
    else
    {
        enumerator = [array objectEnumerator];

        tail = [enumerator nextObject];
        
        while( (str = [enumerator nextObject]) )
        {
            tail = [str commonPrefixWithString:tail options:NSLiteralSearch];
        }
        
        tail = [tail substringFromIndex:[match length]];

        if( tail && ![tail isEqualToString:@""] )
        {
            rl_insert_text([tail cString]);
            rl_redisplay();
        }
        else
        {
            printf("\n");
            enumerator = [array objectEnumerator];
            while( (str = [enumerator nextObject]) )
            {
                printf("%s\n", [str cString]);
            }
            rl_forced_update_display();
        }
    }


    return 0;
}

- (void)exit
{   
    /* FIXME: this is not nice */
    exit(0);
}

- (id)executeScriptNamed:(NSString *)scriptName
{
    STScript *script = [scriptsManager scriptWithName:scriptName];
    id        result = nil;
    
    if(!script)
    {
        [self showError:[NSString stringWithFormat:
                                        @"Unable to find script with name '%@'",
                                        scriptName]];
    }
    else
    {
        NS_DURING
            result = [conversation runScriptFromString:[script source]];
        NS_HANDLER
            [self showException:localException];
        NS_ENDHANDLER
    }
    
    return result;
}
- (void)setPrompt:(NSString *)aString
{
    ASSIGN(prompt, aString);
}
- (NSString *)prompt
{
    return prompt;
}
@end
