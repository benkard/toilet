/**
    stshell
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

#import <StepTalk/StepTalk.h>

#import "STShell.h"

#import <Foundation/NSArray.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSDebug.h>
#import <Foundation/NSException.h>
#import <Foundation/NSProcessInfo.h>
#import <Foundation/NSString.h>

@interface STShellTool:NSObject
{
    STConversation *conversation;
    NSArray        *arguments;
    unsigned int    currentArg;

    NSString       *environmentName;
    NSString       *hostName;
    NSString       *typeName;
    NSString       *languageName;
    
}
- (int)parseArguments;
- (NSString *)nextArgument;
- (void)reuseArgument;
- (void)run;
- (void)printHelp;
@end

@implementation STShellTool
- (int)parseArguments
{
    NSString *arg;
    BOOL      isOption = NO;

    arguments = [[NSProcessInfo processInfo] arguments];

    [self nextArgument];

    while( (arg = [self nextArgument]) )
    {
        isOption = NO;
        if( [arg hasPrefix:@"--"] )
        {
            arg = [arg substringFromIndex:2];
            isOption = YES;
        }
        else if( [arg hasPrefix:@"-"] )
        {
            arg = [arg substringFromIndex:1];
            isOption = YES;
        }
        
        if ([@"help" hasPrefix:arg])
        {
            [self printHelp];
            return 1;
        }
        else if ([@"language" hasPrefix:arg])
        {
            RELEASE(languageName);
            languageName = [self nextArgument];
            if(!languageName)
            {
                [NSException raise:@"STShellToolException"
                            format:@"Language name expected"];
            }
        }
        else if ([@"environment" hasPrefix:arg])
        {
            RELEASE(environmentName);
            environmentName = [self nextArgument];
            if(!environmentName)
            {
                [NSException raise:@"STShellToolException"
                            format:@"Environment name expected"];
            }
        }
        else if ([@"host" hasPrefix:arg])
        {
            RELEASE(hostName);
            hostName = [self nextArgument];
            if(!hostName)
            {
                [NSException raise:@"STShellToolException"
                            format:@"Host name expected"];
            }
        }
        else if ([@"type" hasPrefix:arg])
        {
            RELEASE(typeName);
            typeName = [self nextArgument];
            if(!typeName)
            {
                [NSException raise:@"STShellToolException"
                            format:@"Environment description (type) name expected"];
            }
        }
        else if(!isOption)
        {
            break;
        }
	}
    
    if(arg)
    {
        [self reuseArgument];
    }
    
    return 0;
}

- (NSString *)nextArgument
{
    if(currentArg < [arguments count])
    {
        return [arguments objectAtIndex:currentArg++];
    }
    
    return nil;
}

- (void)reuseArgument
{
    currentArg--;
}
/* Method taken from stexec.m - look there for updates */
- (void)createConversation
{
    STEnvironmentDescription *desc;
    STEnvironment            *environment;
    
    if(environmentName)
    {
        /* user wants to connect to a distant environment */
        conversation = [[STRemoteConversation alloc]
                                    initWithEnvironmentName:environmentName
                                                       host:hostName
                                                   language:languageName];
        if(!conversation)
        {
            NSLog(@"Unable to connect to %@@%@", environmentName, hostName);
            return;
        }
    }
    else
    {
        /* User wants local temporary environment */
        if(!typeName || [typeName isEqualToString:@""])
        {
            environment = [STEnvironment environmentWithDefaultDescription];
        }
        else
        {
            desc = [STEnvironmentDescription descriptionWithName:typeName];
            environment = [STEnvironment environmentWithDescription:desc];
        }

        /* Register basic objects: Environment, Transcript */

        [environment setObject:environment forName:@"Environment"];
        [environment loadModule:@"SimpleTranscript"];
        [environment setCreatesUnknownObjects:YES];
    
        /* FIXME: make this an option */
        [environment setFullScriptingEnabled:YES];

        conversation = [[STConversation alloc] initWithContext:environment
                                                      language:languageName];
    }
}

- (void)run
{	
    STShell       *shell;
    
    [self parseArguments];
    [self createConversation];
          
    if(!languageName || [languageName isEqualToString:@""])
    {
        languageName = [[STLanguageManager defaultManager] defaultLanguage];
    }
    
    [conversation setLanguage:languageName];

    shell = [[STShell alloc] initWithConversation:conversation];
    [shell run];
    
    NSDebugLog(@"Exiting StepTalk shell");    
}

- (void)printHelp
{
    NSProcessInfo *info = [NSProcessInfo processInfo];
    
    printf("%s - StepTalk shell\n"
           "Usage: %s [options]\n\n"
           "Options are:\n"
           "    -help               this text\n"
           "    -language lang      use language lang\n"
           "    -environment env    use scripting environment with name env\n"
           "    -host host          find environment on host\n"
           "    -type desc          use environment description with name 'desc'\n",
           [[info processName] cString],[[info processName] cString]
           );
}

@end


int main(int argc, const char **argv)
{	
    NSAutoreleasePool *pool;
    STShellTool   *tool;

    pool = [NSAutoreleasePool new];

    tool = [[STShellTool alloc] init];
    [tool run];
    
    RELEASE(pool);

    return 0;
}
