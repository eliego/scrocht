#include <stdio.h>
#include <unistd.h>
#include <ApplicationServices/ApplicationServices.h>
#include <CoreFoundation/CoreFoundation.h>
#include <signal.h>

#define SLEEP_PERIOD 5

void changeResolution(int x, int y);
ProcessSerialNumber openUrl(const char* browser, const char* url);


int main (int argc, const char * argv[]) {
	// Parse arguments
	if (argc != 6) {
		printf("Syntax error\n");
		exit(EXIT_FAILURE);
	}
	
	char *browser, *url, *filename, *cmd;
	int xRes, yRes;
	ProcessSerialNumber browserPSN;
	pid_t browserPID;
	
	browser = argv[1];
	url = argv[4];
	filename = argv[5];
	xRes = atoi(argv[2]);
	yRes = atoi(argv[3]);
	
	changeResolution(xRes, yRes);
	browserPSN = openUrl(browser, url);
	
	int sleepPeriod = SLEEP_PERIOD;
	while ((sleepPeriod = sleep(sleepPeriod)) != 0);

	cmd = malloc((40 + strlen(filename)) * sizeof(char));
	sprintf(cmd, "/usr/sbin/screencapture -x -tjpg %s", filename);
	system(cmd);
	
	GetProcessPID(&browserPSN, &browserPID);
	
	// Fuck Apple events, can't get'em right
	kill(browserPID, SIGTERM);
	sleep(1);
	KillProcess(&browserPSN);
    return 0;
}

void changeResolution(int x, int y) {
	CGDirectDisplayID main = CGMainDisplayID();
	CFDictionaryRef mode = CGDisplayBestModeForParameters(main, CGDisplayBitsPerPixel(main), x, y, NULL);
	CGDisplaySwitchToMode(main, mode);
}

ProcessSerialNumber openUrl(const char* browser, const char* url) {
	CFURLRef urlObject;
	CFArrayRef urlArray;
	LSApplicationParameters applicationObject;
	FSRef browserSpec;
	ProcessSerialNumber psn;
	
	urlObject = CFURLCreateWithBytes(kCFAllocatorDefault, (UInt8*)url, strlen(url), kCFStringEncodingNonLossyASCII, NULL);
	urlArray = CFArrayCreate(kCFAllocatorDefault, (void*)&urlObject, 1, NULL);
	
	if (FSPathMakeRef((UInt8*)browser, &browserSpec, NULL) != noErr)
		exit(EXIT_FAILURE);
	
	applicationObject.version = 0;
	applicationObject.flags = kLSLaunchDontAddToRecents | kLSLaunchStartClassic |
		kLSLaunchNewInstance | kLSLaunchAndHideOthers | kLSLaunchHasUntrustedContents;
	applicationObject.application = &browserSpec;
	applicationObject.asyncLaunchRefCon = NULL;
	applicationObject.environment = NULL;
	applicationObject.argv = NULL;
	applicationObject.initialEvent = NULL;
	
	if (LSOpenURLsWithRole(urlArray, kLSRolesAll, NULL, &applicationObject, &psn, 1) != noErr)
		exit(EXIT_FAILURE);
	
	CFRelease(urlArray);
	CFRelease(urlObject);
	
	return psn;
}

