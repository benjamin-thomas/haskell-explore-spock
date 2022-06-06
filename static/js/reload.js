(function () {
    const start = document.head.dataset.srvStart;

    function getLatestStartTime() {
        return fetch('http://localhost:4000')
            .then(res => res.text())
            .then(res => {
                const node = document.createElement('html');
                node.innerHTML = res;

                return node.getElementsByTagName('head')[0].dataset.srvStart;
            })
            .catch(_ => {
                return start;
            });
    }


    function checkReload() {
        getLatestStartTime()
            .then(latestStart => {
                if (start !== latestStart) {
                    document.location.reload();
                }
            });
    }

    setInterval(checkReload, 1000);
})();