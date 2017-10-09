DROP TABLE IF EXISTS labanalysis;
CREATE TABLE labanalysis (
  id INTEGER PRIMARY KEY autoincrement,
  sample_id INTEGER,
  title text NOT NULL,
  description TEXT NOT NULL,
  priority integer NOT NULL,
  email text NOT NULL,

  analysis_status TEXT NOT NULL,
  analysis_run_number INTEGER NOT NULL,

  analysis_start DATETIME,
  analysis_end DATETIME,


  timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO labanalysis(sample_id,
                        title,
                        description,
                         priority,
                          email,
                           analysis_status,
                           analysis_run_number,
                           analysis_start,
                           analysis_end) VALUES (13,
                            "This is a sample Title",
                             "This is a sample description",
                              3,
                               "customer@google.com",
                                "STATUS-STARTED",
                                1,
                                "2017-05-20T12:03:02.200",
                                 "2017-05-20T17:03:02.200");