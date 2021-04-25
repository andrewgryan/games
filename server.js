const path = require("path")
const express = require("express")  
const enableWs = require("express-ws")
const app = express()
const port = process.env.PORT || 8080

// Enable websockets
enableWs(app)


app.use(express.static(__dirname + "/static"))


app.get("/", (req, res) => {
    res.sendFile(path.join(__dirname, "index.html"))
})


// Web socket
app.ws("/ws", (ws, req) => {
    ws.on("message", msg => {
        app.getWss().clients.forEach((client) => {
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
