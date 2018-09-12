'use strict';

// Get outputs of a chain.
var getOutputsByChain = function(chain, onSuccess, onError) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/outputs/' + encodeURIComponent(chain) + '', true);
    xhr.setRequestHeader('Accept', 'application/json');
    xhr.onreadystatechange = function () {
        var res = null;
        if (xhr.readyState === 4) {
            if (xhr.status === 204 || xhr.status === 205) {
                onSuccess();
            } else if (xhr.status >= 200 && xhr.status < 300) {
                try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
                if (res) onSuccess(res);
            } else {
                try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
                if (res) onError(res);
            }
        }
    };
    xhr.send(null);
};


const e = React.createElement;

// Display an issue (to be replaced by Julien's component).
function Issue(props) {
    return e(
        "div",
        {},
        JSON.stringify(props.issue)
    );
}

class Issues extends React.Component {
    constructor(props) {
        super(props);
        this.state = { issues: [], error: null };
        var self = this;

        setInterval(function(){
            getOutputsByChain("issues",
                              function(d){ self.setState({issues: d}); },
                              function(err){ self.setState({error: err}); }
                             );
        }, 1000);
    }

    render() {
        console.log("rendering!");
        console.log(this.state);
        if (this.state.error != null) {
            return "There was an error! " + this.state.error ;
        }

        return this.state.issues.filter(i => i.hasOwnProperty("issue_id")).map(
            (i) => e("div", {key: i.issue_id}, e(Issue, {issue: i}))
        );
    }
}

const domContainer = document.querySelector('#issues_container');
ReactDOM.render(e(Issues), domContainer);
