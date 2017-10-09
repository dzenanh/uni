Firstly, setup the SQL database so that the lab worker can save his work progress.
For ubuntu use this commands.

sudo apt-get update
sudo apt-get install sqlite3

2. Setup Flask (lightweight web-dewelopment framework). Needed to provide REST API of the Laboratory worker.
sudo pip install Flask

3. Run the server with ./main.sh
The output should be: 
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)

After the server is started, you can put lab samples to analysis queue.

REST call to PUT sample into lab-analysis queue is:
http://127.0.0.1:5000/sample/put/211/2/some-description/TITLE/customerEMAIL
with types:

http://127.0.0.1:5000/sample/put/<int:sample_id>/<int:priority>/<string:description>/<string:title>/<string:email>




TO BE CONTINUED... 
