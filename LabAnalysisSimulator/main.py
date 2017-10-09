#!/usr/bin/env python
import sqlite3
import sys
from flask import Flask, g
from helper import LabSample
app = Flask(__name__)

DATABASE = './db/labanalysis.db'

# CASE 1 can not analyse - inform customer
# CASE 2 data NOT ok - RERUN Analysis
# CASE 3 Data OK -> SAVE RESULTS



@app.route("/")
def index():
    save_results()
    return "Hi! My name is <b>AROBOS</b>! Sup? <br><br> Try me out!"


## hand in sample for analysis
@app.route('/sample/put'
           '/<int:sample_id>'
           '/<int:priority>'
           '/<string:description>'
           '/<string:title>'
           '/<string:email>')

def put(sample_id, priority, description, title, email):
    #TODO: implement some 'magic' LOGIC here


    #TODO: save to database with STATUS in workQueue (wait for 5 minutes)
    labSample = LabSample(sample_id, title, description, priority, email, "WAITING_FOR_ANALYSIS", 1)
    put_to_working_queue(labSample)
    print labSample.selectMeQuery()
    return 'Lab Sample ID: %s' % query_db(labSample.selectMeQuery())

    #TODO: check in DB for samples with priority

    #TODO: analyse sample with some randomness factor


## CASE 2 ##
# data NOT ok ##
def rerun_analysis():
    #TODO: recursive call with rerunID inc
    print "reruning analysis"

## CASE 3 ##
# Saving Analysis Results #
def save_results():
    #TODO: save to sqllite
    print "results saved"

## HELPER METHODS ##
def put_to_working_queue(labSample):
    #TODO: save to database
    # insert
    query_db(labSample.toInsertQuery())
    return "SAVED TO QUEUE! I HAVE SO MUCH TO DO :("




############## Sqlite3 DB ###################

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


def query_db(query, args=(), one=False):
    with app.app_context():
        cur = get_db().execute(query, args)
        get_db().commit()
        rv = cur.fetchall()
        cur.close()
        return (rv[0] if rv else None) if one else rv



if __name__ == "__main__":
    app.run()