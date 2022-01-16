
document.addEventListener("click", function (event) {
    const tile = {x: Math.floor((event.layerX-10) / tileSize), y: Math.floor((event.layerY-10) / tileSize)};
    socket.emit("destination", JSON.stringify(tile));
});
