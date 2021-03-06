<html>
  <head>
    <meta http-equiv='refresh' content='60' />
    <link rel='stylesheet' type='text/css' href='style.css' />
    <title>Amarok Control</title>
  </head>
  <body bgcolor='white' color='black'>    
    <h1>Amarok Control RUST</h1>
    <div class="wrapperdiv">
      <div class="musicdiv">
        <table class='music'>
          <tr>
	    <th>Artist</th>
	    <th>Title</th>
	    <th>Album</th>
          </tr>
          <tr>
	    <td>{{ Artist }}</td>
            <td>{{ Title }}</td>
	    <td>{{ Album }}</td>
          </tr>
        </table>
        <h2>Controls</h2>
        <table border='0'>
          <tr>
	    <td><form action='/execute' method='get'><button type='submit' name='what' value='prev'>Previous</button></form></td>
	    <td><form action='/execute' method='get'><button type='submit' name='what' value='next'>Next</button></form></td>
	    <td><form action='/execute' method='get'><button type='submit' name='what' value='play'>Play</button></form></td>
	    <td><form action='/execute' method='get'><button type='submit' name='what' value='pause'>Pause</button></form></td>
	    <td><form action='/execute' method='get'><button type='submit' name='what' value='pp'>PlayPause</button></form></td>
	    <td><form action='/execute' method='get'><button type='submit' name='what' value='stop'>Stop</button></form></td>
          </tr>
        </table>
        <img src="/cover" width="300px" height="300px"></img>
        <p class='status'>Status: {{ StatusM }}
        </p>
      </div>
      
      <div class="seriediv">
        <h2>Serieviewer</h2>
        <p class='title'>Title: {{ TitleS }}<a/p>
            <p class='status'>Status: {{ StatusS }}</p>
            <table border='0'>
              <tr>
	        <td><form action='/execute' method='get'><button type='submit' name='what' value='playnexts'>Play Next Episode in Series</button></form></td>
	        <td><form action='/execute' method='get'><button type='submit' name='what' value='kill'>Quit VLC</button></form></td>
	        <td><form action='/execute' method='get'><button type='submit' name='what' value='killandnext'>Kill and Play Next</button></form></td>
              </tr>
            </table>
      </div>
    </div>
  </body>
</html>
