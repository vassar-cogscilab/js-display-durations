function estimate_frame_rate(callback, duration, trim){
    var last = null;
    var frames = [];

    // skip first frame to get more consistent estimate
    window.requestAnimationFrame(function(){
        window.requestAnimationFrame(nextFrame);
    })

    function nextFrame(timestamp){
        if(last !== null){
            var frame_duration = timestamp - last;
            frames.push(frame_duration);
        }
        last = timestamp;
        var remaining = duration - (timestamp - start);
        if(remaining > 0){
            window.requestAnimationFrame(nextFrame);
        } else {
            done();
        }
    }

    function done(){
        if(trim){
            var sorted = frames.sort();
            var toSum = sorted.slice(Math.round(sorted.length*0.1), Math.round(sorted.length*0.9));        
        } else {
            var toSum = frames;
        }
        var sum = toSum.reduce(function(total, num){ return total + num; });
        callback(sum / toSum.length);
    }
}

