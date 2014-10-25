#!/usr/bin/python -tu

import sys, os, glob, subprocess
from datetime import datetime

BASE_DIR = '/var/lib/cloudway'
CHECK_INTERVAL = 3600       # 1 hour
STALE_TIME = 240 * 86400    # 10 days
DATE_TIME_FORMAT = "%Y-%m-%d %H:%M:%S.%f"

last_access_data = {}
debug = False

def load_last_access_data():
    last_access_data.clear()
    for entry in glob.glob(os.path.join(BASE_DIR, '*')):
        id  = read_file(os.path.join(entry, '.env', 'CLOUDWAY_APP_ID'))
        dns = read_file(os.path.join(entry, '.env', 'CLOUDWAY_APP_DNS'))
        if id and dns:
            atime = read_datetime(os.path.join(entry, 'app', '.last_access'))
            state = read_file(os.path.join(entry, 'app', '.state'))
            if atime is None:
                atime = datetime.now()
                write_datetime(os.path.join(entry, 'app', '.last_access'), atime)
            last_access_data[dns] = {'id':id, 'atime':atime, 'state':state}

def update_last_access_data(data):
    for dns, atime in data.iteritems():
        info = last_access_data.get(dns)
        if info is not None:
            info['atime'] = atime
            write_datetime(os.path.join(BASE_DIR, info['id'], 'app', '.last_access'), atime)
            if debug: print >> sys.stderr, "updating last access time for %s" % info['id']

def idle_staled_apps():
    apps = []
    for info in last_access_data.itervalues():
        if info['state'] == 'STARTED' and (datetime.now() - info['atime']).total_seconds() > STALE_TIME:
            apps.append(info['id'])
            if debug: print >> sys.stderr, "Idling %s" % info['id']
    if len(apps) != 0:
        subprocess.call(['cwctl', 'idle'] + apps)

def read_file(file):
    try:
        with open(file, 'r') as f:
            return f.read().rstrip()
    except:
        return None

def read_datetime(file):
    try:
        with open(file, 'r') as f:
            value = f.read().rstrip()
            return datetime.strptime(value, DATE_TIME_FORMAT)
    except:
        return None

def write_datetime(file, value):
    with open(file, 'w') as f:
        f.write(value.strftime(DATE_TIME_FORMAT))

if __name__ == '__main__':
    data = {}
    last_check_time = datetime.now()

    while True:
        line = sys.stdin.readline()
        if not line: break
        current_time = datetime.now()
        data[line.strip()] = current_time

        if (current_time - last_check_time).total_seconds() > CHECK_INTERVAL:
            load_last_access_data()
            update_last_access_data(data)
            idle_staled_apps()
            data.clear()
            last_check_time = current_time

    # update at end of input
    if len(data) != 0:
        load_last_access_data()
        update_last_access_data(data)