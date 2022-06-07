(function () {

    autoReloadStart = function (serverRestarted) {
        const ws = new WebSocket(`ws://${window.location.host}`);

        ws.onmessage = function (event) {
            var i = Number(event.data);
            if (serverRestarted && i === 1) {
                window.location.reload();
            }
        };

        ws.onclose = function () {
            setTimeout(function () {
                autoReloadStart(true);
            }, 100);
        };
    };

    window.onload = autoReloadStart(false);
}());