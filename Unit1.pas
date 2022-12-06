unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls, WebLib.JSON, jsdelphisystem,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.REST;

type
  TForm1 = class(TWebForm)
    divMain: TWebHTMLDiv;
    WebEdit1: TWebEdit;
    divTabulator: TWebHTMLDiv;
    divChart: TWebHTMLDiv;
    [async] procedure WebEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WebFormShow(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    [async] procedure UpdateChart;
    [async] function GetTrafficData(repo: String): JSValue;
    procedure WebFormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    tabRepos: JSValue;
    tabReposBuilt: Boolean;
    GitHubToken: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.GetTrafficData(repo: String): JSValue;
var
  WebRequest: TWebHTTPRequest;
  WebResponse: TJSXMLHTTPRequest;
  Data: String;
  JSONData: TJSONObject;

begin
  WebRequest := TWebHTTPRequest.Create(Self);
  WebRequest.URL := 'https://api.github.com/repos/'+repo+'/traffic/views';
  WebRequest.Headers.AddPair('Accept','application/vnd.github+json');
  WebRequest.Headers.AddPair('Authorization','Bearer '+Form1.GitHubToken);
  WebResponse := await(TJSXMLHTTPRequest, WebRequest.Perform());
  Data := String(WebResponse.Response);
  try
    JSONData := TJSONObject.ParseJSONValue(Data) as TJSONObject;
    asm
      // this is the original JSON
      var traffic = JSON.parse(Data);

      // Want to convert it into something we can display in a chart
      var convert = {};

      // [{<date> <title> <count>}]
      traffic.views.forEach(function(trafficdate){
        convert[trafficdate.timestamp.substr(0,10)] = trafficdate.uniques;
      });

      Result = JSON.stringify(convert);
    end;
  except on E: Exception do
    begin
      asm
        Result = '{}'
      end;
    end;
  end;
end;

procedure TForm1.UpdateChart;
var
  NumRepos: Integer;
begin
  if (tabReposBuilt) then
  begin

    NumRepos := 0;

    asm
      var allrepodata = {};
      var repodata = {};
      var repolist = [];
      var repo = '';
      var title = '';

      // Figure out if any repositories are currently selected
      var table = pas.Unit1.Form1.tabRepos;
      var rows = table.getSelectedRows();

      NumRepos = rows.length;

      // If there are, we can draw a chart

      if (NumRepos > 0) {

        divChart.classList.remove('d-none');
        divTabulator.classList.replace('h-100','h-50');

        // Get data from all of the repositories

        for (var i = 0; i < NumRepos; i++) {
          repo = rows[i].getCell('full_name').getValue();
          title = rows[i].getCell('name').getValue();

          repodata = JSON.parse(await pas.Unit1.Form1.GetTrafficData(repo));

          for (var trafficdate in repodata) {
            allrepodata[trafficdate] = { ...allrepodata[trafficdate], ...{[title]:repodata[trafficdate]} }
          };

          repolist[i] = title;
        }


        // Now have to reorganize the data for charting, basically populating every combination and
        // ensuring zero values are present where needed.


        // This is the array of dates we're going to use (past 14 days)

        var getDaysArray = function(dtstart, dtend) {
          for(var arr=[],dt=new Date(dtstart); dt<=new Date(dtend); dt.setDate(dt.getDate()+1)){
            arr.push(new Date(dt).toISOString().split('T')[0]);
          }
          return arr;
        };
        var trafficdates = getDaysArray(new Date() - (15 * 24 * 60 * 60 * 1000), new Date() - (-1 * 24 * 60 * 60 * 1000));


        // Recreate the data.  Has the benefit of also sorting it
        // [{date: date, repo1: visitors, repo2: visitors, repo3: visitors}]

        var ChartData = [];
        for (var i = 0; i < trafficdates.length; i++) {
          var values = {};
          for (var j = 0; j < repolist.length; j++) {
            var visitors = 0;
            if (allrepodata[trafficdates[i]] !== undefined) {
              visitors = allrepodata[trafficdates[i]][repolist[j]] || 0;
            }
            values = {...values, ...{date:trafficdates[i],[repolist[j]]:visitors} }
          }
          ChartData[i] = values;
        }


//        console.log(trafficdates);
//        console.log(allrepodata);
//        console.log(ChartData);
//        console.log(repolist);


        // Let's make a D3 Stacked Bar Chart!  This is modified from the following links.
        // One of the main changes is to update the code from D3 v3 to D3 v4
        // https://www.educative.io/answers/how-to-create-stacked-bar-chart-using-d3
        // https://observablehq.com/@stuartathompson/a-step-by-step-guide-to-the-d3-v4-stacked-bar-chart

        var margin = 8;
        var width = divChart.offsetWidth - (margin * 6);
        var height = divChart.offsetHeight - (margin * 6);
        var colors = ["#C9D6DF", "#F7EECF", "#E3E1B2", "#F9CAC8"];
        var parseDate = d3.utcParse("%Y-%m-%d");
        var formatDate = d3.timeFormat("%b-%d");           // Jan-01

        // Replace chart whenever we're here    
        divChart.innerHTML = '';

        // Chart is an SVG image created in the divChart TWebHTMLDiv component
        // Here we're positioning it with a bit of margin
        var svg = d3.select("#divChart")
                    .append("svg")
                    .attr("width", width + (margin * 6))
                    .attr("height", height + (margin * 6))
//                    .attr("transform", "translate("+margin+","+margin+")")
                    .append("g")
                    .attr("width", width - (margin * 8))
                    .attr("height", height - (margin * 8))
                    .attr("transform", "translate("+(margin * 2) +","+(margin * 2)+")");


        // This is the insanity needed to create the stacked portion of the bar chart
        var stack = d3.stack().keys(repolist)(ChartData);

        stack.map((d,i) => {
          d.map(d => {
            d.key = repolist[i]
            return d
          })
          return d
        });
//       console.log(stack);

        // Search the data to figure out what the largest possible y value will be
        var yMax = d3.max(ChartData, d => {
          var val = 0
          for(var k of repolist){
            val += d[k]
          }
          return val
        });
//        console.log(yMax);


        // Deal with the X-Axis
        var x = d3.scaleLinear().domain([0,ChartData.length-1]).range([margin*5,width]);
        var xAxis = d3.axisBottom(x)
                      .ticks(16)
                      .tickFormat((d, i) => formatDate(parseDate(trafficdates[d])));
        svg.append('text')
           .attr('x', width/2)
           .attr('y', height + 30)
           .attr('text-anchor', 'middle')
           .text('UTC Date');


        // Deal with the Y-Axis
        var y = d3.scaleLinear().domain([0, yMax]).range([height,0])
        var yAxis = d3.axisLeft(y);
        svg.append('text')
           .attr('text-anchor', 'middle')
           .attr('transform', 'translate(-8,'+ height/2 + ')rotate(-90)')
           .text('Unique Visitors');


        // Draw the bar charts
        svg.selectAll('g')
           .data(stack).enter()
           .append('g')
          .selectAll('rect')
          .data(d => d).enter()
            .append('rect')
            .attr('x', (d,i) => x(i) - (width/ChartData.length/2))
            .attr('width', width/ChartData.length)
            .attr('height', d => {
               return y(d[0])-y(d[1])
             })
            .attr('y', d => y(d[1]))
            .attr('fill', "#F00")
            .attr('stroke', 'black')
            .attr('stroke-width', 1)
            .append("title")
            .text(function(d,i) {return d.key });  // hover text

        svg.append('g')
          .attr("transform", "translate("+margin * 5+",0)")
          .call(yAxis);

        svg.append('g')
          .attr("transform", "translate(0,"+(height)+")")
          .call(xAxis);

        svg.selectAll("line").style("stroke", "#6c757d");  // Bootsrap secondary color
        svg.selectAll("path").style("stroke", "#6c757d");
        svg.selectAll("text").style("stroke", "white");
        svg.selectAll("text").style("fill", "white");
        svg.selectAll("text").style("stroke-width", "0.2");
        svg.selectAll("text").style("font-size", "10px");
      }

    end;

    if (NumRepos = 0) then
    begin
      divChart.ElementHandle.classList.add('d-none');
      divTabulator.ElementHandle.classList.replace('h-50','h-100');
    end
  end;
end;

procedure TForm1.WebEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  WebRequest: TWebHTTPRequest;
  WebResponse: TJSXMLHTTPRequest;
  Data: String;
  JSONData: TJSONArray;

begin

  if Key = VK_RETURN then
  begin
    GitHubToken := WebEdit1.Text;

    if Length(GitHubToken) < 50 then
    begin
      WebEdit1.Text := '';
      WebEdit1.TextHint := 'Token is too short. Please try again.';
    end
    else
    begin
      WebEdit1.Text := '';
      WebEdit1.TextHint := 'Retrieving Repositories. Please Wait.';

      WebRequest := TWebHTTPRequest.Create(Self);
      WebRequest.URL := 'https://api.github.com/user/repos';
      WebRequest.Headers.AddPair('Accept','application/vnd.github+json');
      WebRequest.Headers.AddPair('Authorization','Bearer '+GitHubToken);
      WebResponse := await(TJSXMLHTTPRequest, WebRequest.Perform());
      Data := String(WebResponse.Response);

      try
        JSONData := TJSONObject.ParseJSONValue(Data) as TJSONArray;
        asm
//          console.log(JSON.parse(Data));
          this.tabRepos.setData(JSON.parse(Data));
          this.tabRepos.selectRow();
        end;
        WebEdit1.Visible := False;
        divTabulator.ElementHandle.classList.remove('d-none');
        UpdateChart;
      except on E: Exception do
        begin
          WebEdit1.Text := '';
          WebEdit1.TextHint := 'Retrieval Failed. Please try again.';
        end;
      end;
    end;
  end;
end;

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  tabReposBuilt := False;

  asm

    var headerMenu = [
      {
        label:"Select All",
        action:function(e, row){
          row.getTable().selectRow();
          pas.Unit1.Form1.UpdateChart();
        }
      },
      {
        label:"Select None",
        action:function(e, row){
          row.getTable().deselectRow();
          pas.Unit1.Form1.UpdateChart();
        }
      }
    ]

    this.tabRepos = new Tabulator("#divTabulator", {
      layout: "fitColumns",
      selectable: true,
      columns: [
        { title: "Repository", field: "name", bottomCalc: "count", widthGrow: 3, headerMenu: headerMenu,
            formatter: function(cell, formatterParams, OnRendered){
              return '<a href='+cell.getRow().getCell('html_url').getValue()+' target="_blank" style="text-decoration: none; color: black;">'+cell.getValue()+'</a>';
            }
        },
        { title: "API URL", field: "url", visible: false },
        { title: "URL", field: "html_url", visible: false },
        { title: "Full Name", field: "full_name", visible: false },
        { title: "Updated", field: "updated_at", widthGrow: 2 },
        { title: "License", field: "license.name", widthGrow: 2 },
        { title: "Language", field: "language", widthGrow: 2 },
        { title: "Private", field: "priv", formatter: "tickCross", bottomCalc: "sum" },
        { title: "Forks", field: "forks", bottomCalc: "sum" },
        { title: "Issues", field: "open_issues_count", bottomCalc: "sum" },
        { title: "Watchers", field: "watchers_count", bottomCalc: "sum" },
        { title: "Stars", field: "stargazers_count", bottomCalc: "sum" }
      ]
    });
    this.tabRepos.on("tableBuilt", function(){
      pas.Unit1.Form1.tabReposBuilt = true;
    });
    this.tabRepos.on("rowSelectionChanged", function(data, rows){
      //rows - array of row components for the selected rows in order of selection
      //data - array of data objects for the selected rows in order of selection
      pas.Unit1.Form1.UpdateChart();
    });
  end;
end;

procedure TForm1.WebFormResize(Sender: TObject);
begin
  UpdateChart();
end;

procedure TForm1.WebFormShow(Sender: TObject);
begin
  WebEdit1.SetFocus;
end;

end.