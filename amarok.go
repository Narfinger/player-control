package main

import (
	"net/http"
	"html/template"
	"fmt"
//	"reflect"
	// "log"
)

import dbus "github.com/guelfey/go.dbus"

type StatusPage struct {
	Title string
	Artist string
	Album string
	StatusM string
	StatusS string
}

func getStatus(object *dbus.Object ) *StatusPage {
	var songinfo map[string]dbus.Variant
	var statusinfo [1][4]int 
	object.Call("org.freedesktop.MediaPlayer.GetMetadata", 0).Store(&songinfo)

	//does not work -.-
	reply := object.Call("org.freedesktop.MediaPlayer.GetStatus", 0).Store(&statusinfo)
	fmt.Println(reply)
	fmt.Println(statusinfo)

	data := StatusPage{Title: "", Artist: "", Album: "", StatusM: "", StatusS: ""}
	if songinfo != nil {
		title := songinfo["title"].String()
		artist := songinfo["artist"].String()
		album := songinfo["album"].String()
		
		data.Title = title
		data.Artist = artist
		data.Album = album
		data.StatusM = "tmpstatus"
		data.StatusS = "seriestatus"
		return &data
	} else {
		return &data
	}
}



func executeHandler(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/tmp/", http.StatusFound)
	return
}

func indexHandler(w http.ResponseWriter, r *http.Request, object *dbus.Object) {
	if r.URL.Path != "/" {
		http.NotFound(w,r)
		return
	}
	t, _ := template.ParseFiles("amarok.html")
	data := getStatus(object)

	t.Execute(w,data)

}

// dbus has signals, i should connect and do stuff with it instead of query everytime!!!
func main() {
	conn, error := dbus.SessionBus()
	if error != nil {
		panic(error)
	}

	object := conn.Object("org.mpris.clementine", "/Player")
	
	getStatus(object)
	
	fmt.Println("Starting Server on port")
	http.HandleFunc("/execute", executeHandler)
	http.HandleFunc("/style.css", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w,r, "style.css")})
	http.HandleFunc("/", func( w http.ResponseWriter, r *http.Request) {
		indexHandler(w,r,object)})

	http.ListenAndServe(":8082", nil)
	fmt.Println("Stopping")
}
