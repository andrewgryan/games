const path = require("path")
const express = require("express")  
const expressWs = require("express-ws")
const app = express()
const port = process.env.PORT || 8080

// Enable websockets
let instance = expressWs(app)

let history = []

app.use(express.static(__dirname + "/static"))

// View engine
app.set("view engine", "ejs")


app.get("/", (req, res) => {
    // res.sendFile(path.join(__dirname, "index.html"))
    let endpoint
    if (process.env.NODE_ENV === "production") {
        endpoint = "wss://sheltered-basin-70535.herokuapp.com/ws"
    } else {
        endpoint = "ws://localhost:8080/ws"
    }
    res.render(path.join(__dirname, "index.ejs"), {
        title: "Quiz",
        endpoint
    })
})


// Web socket
app.ws("/ws", (ws, req) => {
    console.log("websocket connection open")
    history.forEach(msg => {
        ws.send(msg)
    })

    ws.on("message", msg => {
        history.push(msg)
        console.log("websocket message ", msg)
        instance.getWss("/ws").clients.forEach((client) => {
            client.send(msg)
        })

        // ws.send(msg)
    })
    ws.on("close", () => {
        console.log("websocket connection closed")
    })
})

app.listen(port, () => {
    console.log(`listening on: http://localhost:${port}`)
})
