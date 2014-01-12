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
	fmt.Println("start print")
	reply := object.Call("org.freedesktop.MediaPlayer.GetMetadata", 0).Store(&songinfo)
	fmt.Println(reply)

	title := songinfo["title"].String()
	artist := songinfo["artist"].String()
	album := songinfo["album"].String()

	data := StatusPage{Title: title, Artist: artist, Album: album, StatusM: "tmpstatus", StatusS: "seriestatus"}
	fmt.Println(data)
	fmt.Println("end print")
	return &data
}



func executeHandler(w http.ResponseWriter, r *http.Request) {
	http.Redirect(w, r, "/tmp/", http.StatusFound)
	return
}

func indexHandler(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w,r)
		return
	}
	t, _ := template.ParseFiles("amarok.html")
	
	data := StatusPage{Title: "tmptitle", Artist: "tmpartist", Album: "tmpalbum", StatusM: "tmpstatus", StatusS: "seriestatus"}
	t.Execute(w,data)

}

// dbus has signals, i should connect and do stuff with it instead of query everytime!!!

func main() {
	fmt.Println("Starting Server on port")
	http.HandleFunc("/execute", executeHandler)
	http.HandleFunc("/", indexHandler)

	conn, error := dbus.SessionBus()
	if error != nil {
		panic(error)
	}

	object := conn.Object("org.mpris.clementine", "/Player")
	getStatus(object)

	http.ListenAndServe(":8082", nil)
}
