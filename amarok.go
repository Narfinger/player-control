package main

import (
	"net/http"
	"html/template"
	"fmt"
	"errors"
	"time"
	// "os"
	// "reflect"
	// "log"
)

import dbus "github.com/guelfey/go.dbus"

type StatusPage struct {
	Title string
	Artist string
	Album string
	StatusM string
	TitleS string
	StatusS string
}

// controller
func musicPrevious(object *dbus.Object) {
	object.Call("org.freedesktop.MediaPlayer.Prev", 0)
}

func musicNext(object *dbus.Object) {
	object.Call("org.freedesktop.MediaPlayer.Next", 0)
}

func musicPlay(object *dbus.Object) {
	object.Call("org.freedesktop.MediaPlayer.Play", 0)
}

func musicPause(object *dbus.Object) {
	object.Call("org.freedesktop.MediaPlayer.Pause", 0)
}

func musicPlayPause(object *dbus.Object) {
	status, err := getPlayStatus(object)
	if err != nil {
		return
	}
	
	switch status[0] {
	case 0: musicPause(object)
	case 1: musicPlay(object)
	case 2: musicPlay(object)
	}
	//not yet implemented
}

func musicStop(object *dbus.Object) {
	object.Call("org.freedesktop.MediaPlayer.Stop", 0)
}


// description for GetStatus return
// First integer: 0 = Playing, 1 = Paused, 2 = Stopped. 
// Second interger: 0 = Playing linearly , 1 = Playing randomly. 
// Third integer: 0 = Go to the next element once the current has finished playing , 1 = Repeat the current element 
// Fourth integer: 0 = Stop playing once the last element has been played, 1 = Never give up playing
func getPlayStatus(object *dbus.Object) ([4]int32, error) {
	var status [4]int32
	reply := object.Call("org.freedesktop.MediaPlayer.GetStatus", 0)
	if reply == nil {
		return status, errors.New("No Status")
	}

	for key, _ := range status {
		status[key] = reply.Body[0].([]interface{})[key].(int32)
	}
	return status, nil
}

func getSeriePlayStatus() (string) {
	conn, error := dbus.SessionBus()
	if error != nil {
		panic(error)
	}
 	object := conn.Object("org.serieviewer", "/Serieviewer")
	reply := object.Call("org.freedesktop.DBus.Introspectable.Introspect",0)
	if(reply.Err==nil) {
		return "Running"
	} else {
		return "Not Running"
	}
		

	fmt.Println(reply.Err)
	return "TTT"
}

func getSerieTitle(serieobject *dbus.Object) (string) {
	var title string
	serieobject.Call("org.serieviewer.getCurrentName", 0).Store(&title)
	return title
}

func seriePlayNext(serieobject *dbus.Object) {
	serieobject.Call("org.serieviewer.playNextInSerie", 0)
}

func serieKill() {
	conn, error := dbus.SessionBus()
	if error != nil {
		panic(error)
	}
	object := conn.Object("org.mpris.MediaPlayer2.vlc", "/org/mpris/MediaPlayer2")
	reply := object.Call("org.mpris.MediaPlayer2.Quit",0)
}

func serieKillAndNext(serieobject *dbus.Object) {
	serieKill()
	time.Sleep(2 * time.Second)
	seriePlayNext(serieobject)
}
// pages
func getStatus(musicobject *dbus.Object, serieobject *dbus.Object ) *StatusPage {
	var songinfo map[string]dbus.Variant
//	var statusinfo [1][4]dbus.Variant 
	musicobject.Call("org.freedesktop.MediaPlayer.GetMetadata", 0).Store(&songinfo)

	data := StatusPage{Title: "", Artist: "", Album: "", StatusM: "", TitleS: "", StatusS: ""}
	if songinfo != nil {
		title := songinfo["title"].String()
		artist := songinfo["artist"].String()
		album := songinfo["album"].String()
		
		data.Title = title
		data.Artist = artist
		data.Album = album
		data.StatusM = "tmpstatus"
		data.StatusS = "seriestatus"
	}

	status, err := getPlayStatus(musicobject)
	if err == nil {
		switch status[0] {
		case 0: data.StatusM = "Playing"
		case 1: data.StatusM = "Paused"
		case 2: data.StatusM = "Stopped"
		default: data.StatusM = "Error"
		}
	}

	// serie
	data.StatusS = getSeriePlayStatus()
	data.TitleS = getSerieTitle(serieobject)
	return &data

}

func executeHandler(w http.ResponseWriter, r *http.Request, musicobject *dbus.Object, serieobject *dbus.Object) {
	what := r.URL.Query()["what"][0]
	switch what {
	case "prev":  go musicPrevious(musicobject)
	case "next":  go musicNext(musicobject)
	case "play":  go musicPlay(musicobject)
	case "pause": go musicPause(musicobject)
	case "pp":    go musicPlayPause(musicobject)
	case "stop":  go musicStop(musicobject)
	case "playnexts": go seriePlayNext(serieobject)
	case "kill": go serieKill()
	case "killandnext": go serieKillAndNext(serieobject)
	}


	http.Redirect(w, r, "/", http.StatusFound)
	return
}

func indexHandler(w http.ResponseWriter, r *http.Request, musicobject *dbus.Object, serieobject *dbus.Object) {
	if r.URL.Path != "/" {
		http.NotFound(w,r)
		return
	}
	t, _ := template.ParseFiles("amarok.html")
	data := getStatus(musicobject, serieobject)

	t.Execute(w,data)

}

// dbus has signals, i should connect and do stuff with it instead of query everytime!!!
func main() {
	conn, error := dbus.SessionBus()
	if error != nil {
		panic(error)
	}

	musicobject := conn.Object("org.mpris.clementine", "/Player")
	serieobject := conn.Object("org.serieviewer", "/Serieviewer")
	http.HandleFunc("/execute", func (w http.ResponseWriter, r *http.Request) {
		executeHandler(w,r, musicobject, serieobject)})
	http.HandleFunc("/style.css", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w,r, "style.css")})
	http.HandleFunc("/", func( w http.ResponseWriter, r *http.Request) {
		indexHandler(w,r, musicobject, serieobject)})
	

	fmt.Println("Starting on port 8082")
	http.ListenAndServe(":8082", nil)
	fmt.Println("Stopping")
}


// this might be good for signal stuff
	// err := conn.BusObject().Call("org.mpris.clementine", 0,
	// 	"type='signal',path='/Player',interface='org.freedesktop.MediaPlayer.StatusChange'")
	
	// fmt.Println(err)
	// if err != nil {
	// 	os.Exit(1)
	// }


	// c := make(chan *dbus.Signal, 10)
	// conn.Signal(c)
	// fmt.Println("connected")
	// for v := range c {
	// 	fmt.Println(v)
	// }
