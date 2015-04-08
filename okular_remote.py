#!/usr/bin/python
import dbus
import psutil
import subprocess
import sys
import time

def main(argv):
    # TODO: function this
    bus = dbus.SessionBus()
    plist = psutil.get_process_list()
    try:
        plist_okular = [(p.pid,p.name) for p in plist if p.name.startswith('okular-preview')]
    except AttributeError:
        plist_okular = [(p.pid,p.name()) for p in plist if p.name().startswith('okular-preview')]
    if plist_okular == []:
        # hack to start maximized on any conceivable screen
        subprocess.call(['bash','-c','nohup okular-preview -geometry 2000x2000+0+0 2>&1 >> /dev/null &'])
        time.sleep(0.5)
        plist = psutil.get_process_list()
        try:
            plist_okular = [(p.pid,p.name) for p in plist if p.name.startswith('okular-preview')]
        except AttributeError:
            plist_okular = [(p.pid,p.name()) for p in plist if p.name().startswith('okular-preview')]
    pid_okular = [p[0] for p in plist_okular]
    pid = pid_okular[0]
    remote_object = bus.get_object("org.kde.okular-" + str(pid), # Connection name
                                   "/okular" # Object's path
    )
    remote_object.openDocument(argv[1])
    remote_object.slotGotoFirst()
    print argv
    print dir(remote_object)
    if len(argv) > 2:
        remote_object.goToPage(dbus.UInt32(argv[2]))
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
