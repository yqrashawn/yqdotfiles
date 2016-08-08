#! /usr/local/bin/python
import os
import sys
import commands
import alfred

def execute(cmd):
    return commands.getstatusoutput(cmd)[1]

def get_volume():
    return execute("""osascript -e 'output volume of (get volume settings)'""")

def get_muted():
    return execute("""osascript -e 'output muted of (get volume settings)'""")

def set_volume(vol):
    os.system("""osascript -e 'set volume output volume %d'""" % vol)

def set_muted(muted):
    os.system("""osascript -e 'set volume output muted %s'""" % muted)

def get_volume_summary():
    if get_muted() == 'true':
        return "Muted, original volume: " + get_volume() + "%"
    else:
        return "Current volume: " + get_volume() + "%"

def parse_volume(s, default = 10):
    try:
        return int(s)
    except:
        return default

def process_query(q):
    results = [] 
    if(len(q) == 0):
        results.append(alfred.Item(
            attributes = {'arg': '', 'valid': 'false'},
            title = get_volume_summary(),
            subtitle = ""
        ))
    op = q[0].lower() if len(q) > 0 else ""

    if op == "":
        results.append(alfred.Item(
            attributes = {'arg': '', 'valid': 'false'},
            title = "Set Volume to ...",
            subtitle = "vol ${num}"
        ))
    else:
        try:
            new =  int(op)
        except:
            new = -1
        if new >= 0:
            results.append(alfred.Item(
                attributes = {'arg': str(new), 'autocomplete': ''},
                title = "Set Volume: %d%%" % new,
                subtitle = "vol ${num}"
            ))

    if get_muted() != 'true':
        if('mute'.startswith(op)):
            results.append(alfred.Item(
                attributes = {'arg': 'mute', 'autocomplete' : 'mute'},
                title = "Mute",
                subtitle = "vol mute"
            ))
    else:
        if('unmute'.startswith(op)):
            results.append(alfred.Item(
                attributes = {'arg': 'unmute', 'autocomplete': 'unmute'},
                title = "Unumte",
                subtitle = "vol unmute"
            ))
    if('up'.startswith(op)):
        if len(q) >= 2:
            val = parse_volume(q[1], 10)
            arg = 'up ' + str(val)
        else:
            val = 10
            arg = 'up 10'
        results.append(alfred.Item(
            attributes = {'arg': arg, 'autocomplete': 'up'},
            title = "Volume up by %d%%" % val,
            subtitle = "vol up ${num}",
            subtitleCmd = "Hold cmd to double the increament."
        ))
    if('down'.startswith(op)):
        if len(q) >= 2:
            val = parse_volume(q[1], 10)
            arg = 'down ' + str(val)
        else:
            val = 10
            arg = 'down 10'
        results.append(alfred.Item(
            attributes = {'arg': arg, 'autocomplete': 'down'},
            title = "Volume down by %d%%" % val,
            subtitle = "vol down ${num}",
            subtitleCmd = "Hold cmd to double the decreasement."
        ))
    if('max'.startswith(op)):
        results.append(alfred.Item(
            attributes = {'arg': '100', 'autocomplete': 'max'},
            title = "Volume Max",
            subtitle = "vol max"
        ))
    if('low'.startswith(op)):
        results.append(alfred.Item(
            attributes = {'arg': '25', 'autocomplete': 'low'},
            title = "Low Volume: 25%",
            subtitle = "vol low"
        ))
    if('mid'.startswith(op)):
        results.append(alfred.Item(
            attributes = {'arg': '50', 'autocomplete': 'mid'},
            title = "Middle Volume: 50%",
            subtitle = "vol mid"
        ))
    if('high'.startswith(op)):
        results.append(alfred.Item(
            attributes = {'arg': '75', 'autocomplete': 'high'},
            title = "Hidgh Volume: 75%",
            subtitle = "vol high"
        ))

    if(len(q) > 0):
        results.append(alfred.Item(
            attributes = {'arg': '', 'valid': 'false'},
            title = get_volume_summary(),
            subtitle = ""
        ))
    xml = alfred.xml(results) # compiles the XML answer
    alfred.write(xml)

if __name__ == "__main__":
    process_query(sys.argv[1:])