#!/usr/bin/env python
import time
import sqlite3
from datetime import datetime, timedelta
from flask import Flask, g
app = Flask(__name__)

DATABASE = './labanalysis.db'

def get_db():
    db = getattr(g, '_database', None)
    if db is None:
        db = g._database = sqlite3.connect(DATABASE)
    return db

@app.teardown_appcontext
def close_connection(exception):
    db = getattr(g, '_database', None)
    if db is not None:
        db.close()


def execute_script(file):
    with app.app_context():
        db = get_db()
        with app.open_resource(file, mode='r') as f:
            db.cursor().executescript(f.read())
        db.commit()


def query_db(query, args=(), one=False):
    with app.app_context():
        cur = get_db().execute(query, args)
        get_db().commit()
        rv = cur.fetchall()
        cur.close()
        return (rv[0] if rv else None) if one else rv


print "initialising DB..."
execute_script('labanalysis-schema.sql')
print "DB ready!"


print "Inserting TEST data ..."
execute_script('labanalysis-testdata.sql')
print "DONE!"

print "trying out some 'on the fly' inserts..."

for x in xrange(1,20):
    s = str(x)
    print "inserting " + s
    unix = int(time.time())
    datetime_string = str(datetime.fromtimestamp(unix).strftime('%Y-%m-%d %H:%M:%S'))
    #query_db('INSERT INTO labanalysis VALUES ('+s+', "This is a sample Title"'+s+', "This is a sample description2", 2, "customer'+s+'@fit.com", "STATUS-STARTED", 1, '+datetime_string+','+datetime_string+');')
    query_db('INSERT INTO labanalysis(sample_id,title, description, priority,  email, analysis_status, analysis_run_number,  analysis_start,analysis_end) VALUES ('+s+', "This is a sample Title'+s+'", "This is a sample description'+s+'", 2, "customer@fit.com", "STATUS-STARTED", 1, "'+datetime_string+'", "2017-07-21T16:03:02.200");')
print "DONE!"


print "trying out some 'on the fly' inserts..."
unix = int(time.time())
datetime_string = str(datetime.fromtimestamp(unix).strftime('%Y-%m-%d %H:%M:%S'))
#query_db('INSERT INTO labanalysis VALUES ('+s+', "This is a sample Title"'+s+', "This is a sample description2", 2, "customer'+s+'@fit.com", "STATUS-STARTED", 1, '+datetime_string+','+datetime_string+');')
query_db('INSERT INTO labanalysis(sample_id,title, description, priority,  email, analysis_status, analysis_run_number,  analysis_start,analysis_end) VALUES (524, "This is a NEW sample TitleZZ", "This is a NEW sample description", 2, "5customer@fit.com", "STATUS-STARTED-BUG", 1, "'+datetime_string+'", "2017-07-21T16:03:02.200");')
print "DONE!"


print "TESTING SELECT"
for sample in query_db('SELECT * FROM labanalysis LIMIT 100'):
    #print sample['title'], 'has the sample id', sample['sample_id'], ' with status:', sample['analysis_status']
    print sample
print "DONE!"


