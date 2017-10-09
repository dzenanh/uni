class LabSample:
    #meta = 'xXx'
    def __init__(self, sample_id,
                        title,
                        description,
                         priority,
                          email,
                           analysis_status,
                           analysis_run_number):
        self.sample_id = str(sample_id)
        self.title = title
        self.description = description
        self.priority = str(priority)
        self.email = email
        self.analysis_status = analysis_status
        self.analysis_run_number = str(analysis_run_number)

    def toString(self):
        print self.sample_id,",",self.title, ","+self.description+",", self.priority, ",", self.email, ",", self.analysis_status,",",self.analysis_run_number

    def toInsertQuery(self):
        return 'INSERT INTO labanalysis (sample_id,title, description, priority,  email, analysis_status, analysis_run_number) VALUES ('+self.sample_id+', "'+self.title+'", "'+self.description+'", '+self.priority+', "'+self.email+'", "'+self.analysis_status+'", '+self.analysis_run_number+');'

    def selectMeQuery(self):
        return 'SELECT * FROM labanalysis WHERE sample_id='+self.sample_id