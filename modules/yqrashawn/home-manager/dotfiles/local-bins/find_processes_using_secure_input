#!/usr/bin/env python
"""
If macOS detects that you're typing sensitive data (e.g. a password), it prevents
other apps from reading keystrokes.  This is normally a good thing, but it's mildly
annoying if you use an app like TextExpander, and macOS doesn't notice that it's
no longer using Secure Input.  Then your keyboard shortcuts stop working!

This script will print a list of processes that have Secure Input enabled, e.g.:

    The following processes are using Secure Input:
      113 loginwindow
      302 Safari
      519 Firefox

It relies on two interesting commands:

    ioreg -a -l -w 0
        prints the I/O Kit Registry in XML format, which includes information about
        a process using Secure Input in the kCGSSessionSecureInputPID key

    ps -c -o command= -p <pid>
        prints the executable name associated with a process ID (pid), which is
        used for the output

See https://alexwlchan.net/2021/04/secure-input/

"""

import plistlib
import subprocess


def find_dicts_in_tree(d):
    """
    Traverses the values of d, returning everything that looks dict-like.

    Ideally I'd have a better idea of the structure of the output of ioreg,
    but I don't care to spend that much time on this problem.
    """
    if isinstance(d, dict):
        yield d
        for dict_value in d.values():
            for dv in find_dicts_in_tree(dict_value):
                yield dv
    elif isinstance(d, list):
        for list_item in d:
            for lv in find_dicts_in_tree(list_item):
                yield lv
    else:
        pass


def executable_name(pid):
    """
    Returns the executable name associated with a given pid.
    """
    return subprocess.check_output([
        "ps",
        "-c",              # Only show the executable name, not the full command line
        "-o", "command=",  # Only show the command column, no header
        "-p", str(pid)     # Only show the given process ID
    ]).strip().encode("utf8")


if __name__ == "__main__":
    ioreg_output = subprocess.check_output([
        "ioreg",
        "-a",      # Archive the output in XML
        "-l",      # Show properties for all displayed objects
        "-w", "0"  # Unlimited line width
    ])

    try:
        plist = plistlib.loads(ioreg_output, fmt=plistlib.FMT_XML)
    except AttributeError:  # Python 2
        plist = plistlib.readPlistFromString(ioreg_output)

    process_ids = set()
    for d in find_dicts_in_tree(plist):
        if "kCGSSessionSecureInputPID" in d:
            process_ids.add(d["kCGSSessionSecureInputPID"])

    sorted_pids = [
        str(pid)
        for pid in sorted(int(p) for p in process_ids)
    ]

    print("The following processes are using Secure Input:")
    if sorted_pids:
        subprocess.check_call([
            "ps",
            "-c",                        # Only show the executable name, not the full command line
            "-o", "pid=,command=",        # Only show the PID/command column, no header
            "-p", ",".join(sorted_pids)  # Only get the selected processes
        ])
