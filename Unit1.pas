unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls, jsdelphisystem,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, WEBLib.WebTools,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.REST, WEBLib.JSON, WEBLib.ExtCtrls, WEBLib.Storage, System.DateUtils;

type
  TForm1 = class(TWebForm)
    divMain: TWebHTMLDiv;
    WebEdit1: TWebEdit;
    divTabulator: TWebHTMLDiv;
    divChart: TWebHTMLDiv;
    WebTimer1: TWebTimer;
    procedure WebFormShow(Sender: TObject);
    [async] procedure WebFormCreate(Sender: TObject);
    procedure WebFormResize(Sender: TObject);
    [async] procedure WebTimer1Timer(Sender: TObject);
    [async] procedure UpdateTable;
    [async] procedure WebEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    [async] procedure RefreshTableData;
    [async] procedure UpdateChart;
    [async] procedure UpdateCalendar;
    [async] function GetTrafficData(repo: String): JSValue;
    procedure divChartDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    tabRepos: JSValue;
    tabReposBuilt: Boolean;

    PauseChartUpdates: Boolean;
    Table_Data: String;
    Table_Data_Age: TDateTime;

    GitHubToken: String;

    automate: Boolean;

    Param_Mode: String;
    Param_GitHubToken: String;
    Param_Calendar: String;

    Param_Top: Integer;
    Param_Left: Integer;
    Param_Width: Integer;
    Param_Height: Integer;

    Param_FontSize: Integer;
    Param_Background: String;
    Param_Scale: String;

    Highlight: String;
    CurrentCalendar: Integer;
    NextCalendar: Integer;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.divChartDblClick(Sender: TObject);
begin
  window.open(window.location.href,'_blank');
end;

function TForm1.GetTrafficData(repo: String): JSValue;
var
  WebRequest: TWebHTTPRequest;
  WebResponse: TJSXMLHTTPRequest;
  Data: String;
//  JSONData: TJSONObject;
  RepoAge: TDateTime;
  RequestData: Boolean;

begin

  // Assume we'll be asking for data
  RequestData := True;

  // If we already have data for this cached for this repo, then use it
  if (TWebLocalStorage.getValue('GAE.Repo.Data.'+Repo) <> '') then
  begin
    if (TWebLocalStorage.getValue('GAE.Repo.DataAge.'+Repo) <> '') then
    begin
      RepoAge := StrToFloat(TwebLocalStorage.GetValue('GAE.Repo.DataAge.'+Repo));
      Result := TWebLocalStorage.getValue('GAE.Repo.Data.'+Repo);
      if MinutesBetween(Now, RepoAge) < 60 then
      begin
        RequestData := False;
//        console.log('Retrieving ['+Repo+'] Data from Cache');
      end;
    end;
  end;

  // Otherwise, go and get it
  if RequestData then
  begin
//    console.log('Retrieving ['+Repo+'] Data from GitHub');
    WebRequest := TWebHTTPRequest.Create(Self);
    WebRequest.URL := 'https://api.github.com/repos/'+repo+'/traffic/views?per_page=50';
    WebRequest.Headers.AddPair('Accept','application/vnd.github+json');
    WebRequest.Headers.AddPair('Authorization','Bearer '+GitHubToken);
    WebResponse := await(TJSXMLHTTPRequest, WebRequest.Perform());

    Data := String(WebResponse.Response);
    try
//      JSONData := TJSONObject.ParseJSONValue(Data) as TJSONObject;
      asm

        // this is the original JSON
        var traffic = JSON.parse(Data);
//      console.log(traffic);
        // Want to convert it into something we can display in a chart
        var convert = {};

        // [{<date> <title> <count>}]
        traffic.views.forEach(function(trafficdate){
          convert[trafficdate.timestamp.substr(0,10)] = trafficdate.uniques;
        });

        Data = JSON.stringify(convert);
      end;
    except on E: Exception do
      begin
        asm
          Data = '{}'
        end;
      end;
    end;

    if Data = '{}' then
    begin
      TWebLocalStorage.RemoveKey('GAE.Repo.Data.'+Repo);
      TWebLocalStorage.RemoveKey('GAE.Repo.DataAge.'+Repo);
    end
    else
    begin
      TWebLocalStorage.SetValue('GAE.Repo.Data.'+Repo, Data);
      TWebLocalStorage.SetValue('GAE.Repo.DataAge.'+Repo, FloatToStr(Now));
    end;

    Result := Data;
  end;
end;

procedure TForm1.UpdateCalendar;
begin

  // Set Calendar size and position
  divChart.Top := Param_Top;
  divChart.Left := Param_Left;
  divChart.Width := Param_Width;
  divChart.Height := Param_Height;
  divChart.HTML.Text := '';
  divChart.ElementClassName := 'position-absolute Calendar d-flex gap-1 flex-column flex-wrap flex-fill';
  divChart.ElementHandle.style.setProperty('transform-origin','top left');
  divChart.ElementHandle.style.setProperty('transform','scale('+Param_Scale+')');

  {$IFNDEF WIN32}
  asm {
    pas.Unit1.Form1.NextCalendar = new Date().getUTCDay()
  } end;
  {$ENDIF}

  if CurrentCalendar <> NextCalendar then
  begin
    if CurrentCalendar = -1  then
    begin
      {$IFNDEF WIN32}
      asm {
        divChart.addEventListener('click', () =>
          window.open(window.location.href, '_blank').focus()
        );
      } end;
      {$ENDIF}
    end;

    CurrentCalendar := NextCalendar;

    {$IFNDEF WIN32}
    asm {


      async function sleep(msecs) {
        return new Promise((resolve) =>setTimeout(resolve, msecs));
      }
      await sleep(100);

      divMain.style.setProperty('background-color', this.Param_Background,'important');
      divMain.style.setProperty('position', 'absolute');
      divMain.style.setProperty('width', '100%');
      divMain.style.setProperty('height', '100%');

//              contributionsCollection(from: "${start_date}", to: "${finish_date}") {

      async function Get_GitHub_Data(GITHUB_ACCOUNT, GITHUB_TOKEN, start_date, finish_date) {

        const QUERY = `
          query {
            user(login: "${GITHUB_ACCOUNT}") {
              contributionsCollection {
                contributionCalendar {
                  totalContributions
                  weeks {
                    contributionDays {
                      date
                      contributionCount
                    }
                  }
                }
              }
            }
          }
        `;

//        console.log('Query: ', QUERY);

        const options = {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `bearer ${GITHUB_TOKEN}`
          },
          body: JSON.stringify({ query: QUERY })
        };

//        console.log('Options: ', options);

        return fetch('https://api.github.com/graphql', options)
          .then(res => {
//            console.log('Response: ', res);
            return res.json();
          })
          .then(data => {
//            console.log('Data: ', data);
            let contributions = [];
            data.data.user.contributionsCollection.contributionCalendar.weeks.forEach(week => {
              week.contributionDays.forEach(day => {
                contributions.push({
                  date: day.date,
                  count: day.contributionCount
                });
              });
            });
            return contributions;
          })
          .catch(err => {
            console.log(err);
          });
      }

      var calendardata = (await Get_GitHub_Data(
        this.Param_Calendar,
        this.Param_GitHubToken,
        new Date(new Date().setFullYear(new Date().getFullYear() - 1)).toJSON(),
        new Date().toJSON()
      ));

      var i = 0;
      divChart.replaceChildren();
      var started = false;
      while (i < calendardata.length) {
        if ((started == false) && (new Date(calendardata[i].date).getUTCDay() == 0)) {
          started = true;
        }

        if (started == true) {
//        console.log(calendardata[i].date+' '+calendardata[i].count);
          var cal = document.createElement('div');
          if (calendardata[i].count == 0) {
            cal.classList.add('None');
          } else if (calendardata[i].count < 6) {
            cal.classList.add('Low');
          } else if (calendardata[i].count < 11) {
            cal.classList.add('Medium');
          } else {
            cal.classList.add('High');
            cal.innerHTML = calendardata[i].count;
          }
          if (new Date(calendardata[i].date).getUTCDate() == 1) {
            cal.classList.add('First');
          }

          cal.setAttribute('title',calendardata[i].date+': '+calendardata[i].count);
          divChart.appendChild(cal);
        }

        i++;
      }
    }  end;
    {$ENDIF}
  end;

  divChart.Visible := True;
end;

procedure TForm1.UpdateChart;
var
  NumRepos: Integer;
begin

  if PauseChartUpdates then exit;
//  console.log('updating chart');


  while not(tabReposBuilt) do
  begin
    asm
      async function sleep(msecs) {
        return new Promise((resolve) =>setTimeout(resolve, msecs));
      }
      await sleep(100);
    end;
  end;

//  console.log('drawing chart');

  NumRepos := 0;

  divChart.Top := Param_Top;
  divChart.Left := Param_Left;
  divChart.Width := Param_Width;
  divChart.Height := Param_Height;

  asm
    divMain.style.setProperty('background-color', this.Param_Background,'important');
    divMain.style.setProperty('position', 'absolute');
    divMain.style.setProperty('width', '100%');
    divMain.style.setProperty('height', '100%');

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

//      if (pas.Unit1.Form1.automate == true) {
//        divTabulator.classList.replace('h-100','d-none');

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

      // More data cleaning.  Remove days without data at start
      for (var i = 0; i < ChartData.length; i++) {
        var empty = true;
        Object.keys(ChartData[i]).forEach(function(key) {
          if ((key !== 'date') && (parseInt(ChartData[i][key]) !== 0)) {
            empty = false;
          }
        });
        if ((empty == true) && (i == 0)) {
//          console.log('Removing empty starting date: '+ChartData[i].date);
          ChartData.splice(i,1);
          i = i - 1;
        }
      }
      // More data cleaning.  Remove days without data at end
      for (var i = ChartData.length - 1; i >= 0; i--) {
        var empty = true;
        Object.keys(ChartData[i]).forEach(function(key) {
          if ((key !== 'date') && (parseInt(ChartData[i][key]) !== 0)) {
            empty = false;
          }
        });
        if ((empty == true) && (i == ChartData.length - 1)) {
//          console.log('Removing empty ending date: '+ChartData[i].date);
          ChartData.splice(i,1);
          i = i - 1;
        }
        if ((empty == true) && (i == ChartData.length - 1)) {
//          console.log('Removing empty ending date: '+ChartData[i].date);
          ChartData.splice(i,1);
          i = i - 1;
        }
      }

      table.blockRedraw();
      var r = table.getSelectedRows();
      for (var i = 0; i < r.length; i++) {
        r[i].getCell('pageviews').setValue(0);
      }
      for (var i = 0; i < ChartData.length; i++) {
        var list = ChartData[i];
        for (var rep in list) {
          var r = table.searchRows('name','=',rep);
          if (r.length == 1) {
            var c = r[0].getCell('pageviews').getValue();
            r[0].getCell('pageviews').setValue(c + list[rep]);
          }
        }
      }
      table.restoreRedraw();

//        var keysSorted = Object.keys(list).sort(function(a,b){return list[a]-list[b]})
//        console.log(keysSorted);
//      }

//        console.log(trafficdates);
//        console.log(allrepodata);
//        console.log(ChartData);
//        console.log(repolist);



      // Let's make a D3 Stacked Bar Chart!  This is modified from the following links.
      // One of the main changes is to update the code from D3 v3 to D3 v4
      // https://www.educative.io/answers/how-to-create-stacked-bar-chart-using-d3
      // https://observablehq.com/@stuartathompson/a-step-by-step-guide-to-the-d3-v4-stacked-bar-chart

        var margin = 0;
//        if (pas.Unit1.Form1.automate) {
//          margin = 2;
////          divChart.style.setProperty('background-color', bgcolor,'important');
////          divMain.style.setProperty('background-color', bgcolor,'important');
////          divChart.style.setProperty('border-radius', '6px','important');
////          divMain.style.setProperty('border-radius', '6px','important');
////          document.body.style.setProperty('background-color', bgcolor,'important');
//        }
        var width = pas.Unit1.Form1.Param_Width - (margin * 6);
        var height = pas.Unit1.Form1.Param_Height - (margin * 6);
        var colors = ["#C9D6DF", "#F7EECF", "#E3E1B2", "#F9CAC8"];
        var parseDate = d3.utcParse("%Y-%m-%d");
        var formatDate = d3.timeFormat("%b-%d");           // Jan-01
        var ParamX = pas.Unit1.Form1.Param_Left;
        var ParamY = pas.Unit1.Form1.Param_Top;

        // Replace chart whenever we're here    
        divChart.replaceChildren();

        // Chart is an SVG image created in the divChart TWebHTMLDiv component
        // Here we're positioning it with a bit of margin
        var svg = d3.select("#divChart")
                    .append("svg")
                    .attr("width", '100%')
                    .attr("height", '100%')

//                    .attr("width", width + (margin * 6) + ParamX)
//                    .attr("height", height + (margin * 8) + ParamY)
//                    .attr("transform", "translate("+margin+","+margin+")")
                    .append("g")
                    .attr("width", '100%')
                    .attr("height", '100%')
                    .attr("transform", "translate(12,24)");


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
        var x = d3.scaleLinear()
                  .domain([-0.5,ChartData.length-0.5])
                  .range([0,width-50]);
        var xAxis = d3.axisBottom(x)
                      .ticks(ChartData.length)
                      .tickFormat((d, i) => formatDate(parseDate(trafficdates[d])));


//        svg.append('text')
//           .attr('x', width/2)
//           .attr('y', height + 30)
//           .attr('text-anchor', 'middle')
//           .text('UTC Date')
//           .style('font-size', '12px');



        // Deal with the Y-Axis
        var y = d3.scaleLinear().domain([0, yMax]).range([height - ParamY-45, -20])
        var yAxis = d3.axisLeft(y);

//        if (pas.Unit1.Form1.automate == false) {
//          svg.append('text')
//             .attr('text-anchor', 'middle')
//             .attr('transform', 'translate(-8,'+ height/2 + ')rotate(-90)')
//             .text('Unique Visitors')
//             .style('font-size', '12px');
//        }


        // Draw the bar charts
        svg.selectAll('g')
           .data(stack).enter()
           .append('g')
           .selectAll('rect')
           .data(d => d)
           .enter()
           .each(function(p,j) {
             d3.select(this)
               .append('rect')
               .on("click", function(d) {
                 if (!pas.Unit1.Form1.Highlight.includes('['+p.key+']')) {
                   pas.Unit1.Form1.Highlight += '['+p.key+']';
                 } else {
                   pas.Unit1.Form1.Highlight = pas.Unit1.Form1.Highlight.replace('['+p.key+']','');
                 }
                 pas.Unit1.Form1.UpdateChart();
               })
               .attr('x', (d,i) => x(j) - (width/ChartData.length/2))
               .attr('y', d => y(d[1]))
               .attr('width', (width / (ChartData.length+1.75)))
               .attr('height', d => {
                 return y(d[0])-y(d[1])
               })
               .attr('fill', function(){
                 // Highlight = Dark Green
                 if (pas.Unit1.Form1.Highlight.includes('['+p.key+']')) {
                   return '#080';
                 } else {
                   // Get count for past 14 days
                   var pv = pas.Unit1.Form1.tabRepos.searchRows('name','=',p.key)[0].getCell('pageviews').getValue();
                   // Average > 3 per day = Red
                   if (pv >= 42) {return '#F00';}
                   // Average >1 1 per day = Maroon
                   else if (pv >= 14) {return '#800';}
                   // Average < 1 per day = Darker Red
                   else {return '#500';}
                 }
               })
               .attr('stroke', 'black')
               .attr('stroke-width', 1)
               .append("title")
               .text(function(d,i) {
                 return d.key+' - '+d.data[d.key]
               });
             d3.select(this)
               .append("text")
               .attr('x', (d,i) => x(j) - 2)
               .attr('y', d => y(d[1])+(y(d[0])-y(d[1]))/2+(parseInt(pas.Unit1.Form1.Param_FontSize) / 3))
               .attr('width', (width/(ChartData.length+2)))
               .attr('height', d => {
                 return y(d[0])-y(d[1])
               })
               .attr("text-anchor", "middle")
               .attr("dominant-baseliner", "middle")
               .attr("pointer-events", "none")
               .style("font-size", parseInt(pas.Unit1.Form1.Param_FontSize)+"px")
               .attr("fill", "white")
               .text( d => {
                 // Too small to fit?
                 if (( y(d[0])-y(d[1]) ) <= pas.Unit1.Form1.Param_FontSize) {
                  return ''
                 }
                 else {
                   return d.key.split('-').pop().replace(/[^A-Z]+/g, "")
                 }
               })
             });


//        if (pas.Unit1.Form1.automate == false) {
//          svg.append('g')
//            .attr("transform", "translate("+margin * 5+",0)")
//            .call(yAxis);
//
//          svg.append('g')
//            .attr("transform", "translate(0,"+(height)+")")
//            .call(xAxis);
//        }

        svg.selectAll("line").style("stroke", "#6c757d");  // Bootsrap secondary color
        svg.selectAll("path").style("stroke", "#6c757d");

        svg.selectAll("text").style("stroke", "white");
        svg.selectAll("text").style("fill", "white");
        svg.selectAll("text").style("stroke-width", "0.2");
     }
  end;

  divChart.Visible := True;

  if (NumRepos = 0) then
  begin
    divChart.ElementHandle.classList.add('d-none');
    divTabulator.ElementHandle.classList.replace('h-50','h-100');
  end
end;

procedure TForm1.UpdateTable;
begin
  while not(tabReposBuilt) or (Table_Data = '') do
  begin
//    if table_data = '' then console.log('waiting for table');
//    if not(tabReposBuilt) then console.log('waiting for table');

    asm
      async function sleep(msecs) {
        return new Promise((resolve) =>setTimeout(resolve, msecs));
      }
      await sleep(100);
    end;
  end;


  asm
    this.tabRepos.setData(JSON.parse(this.Table_Data)).then(async function(){
      pas.Unit1.Form1.PauseChartUpdates = true;
      pas.Unit1.Form1.tabRepos.selectRow();
      pas.Unit1.Form1.PauseChartUpdates = false;
      await pas.Unit1.Form1.UpdateChart();
      pas.Unit1.Form1.PauseChartUpdates = true;
      pas.Unit1.Form1.tabRepos.deselectRow();
      pas.Unit1.Form1.tabRepos.setSort('pageviews','desc');
      for (var i = 1; i <= pas.Unit1.Form1.tabRepos.getDataCount(); i++) {
        pas.Unit1.Form1.tabRepos.getRowFromPosition(i).select();
      }
      pas.Unit1.Form1.PauseChartUpdates = false;
      pas.Unit1.Form1.UpdateChart();
    });
  end;
end;

procedure TForm1.WebEdit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin

    // This is the token we'll need to use to authenticate with GitHub
    GitHubToken := WebEdit1.Text;

    // Save the token so we don't have to enter it each time
    TWebLocalStorage.SetValue('GAE.G', GitHubtoken);

    // Get table data - either from cache or from GitHub
    await(RefreshTableData);

    // If we've got table data, then show it
    if Table_Data <> '' then
    begin
      UpdateTable;
      WebEdit1.Visible := False;
      Form1.divTabulator.ElementHandle.classList.remove('d-none');

      UpdateChart;
    end;

  end;
end;

procedure TForm1.RefreshTableData;
var
  WebRequest: TWebHTTPRequest;
  WebResponse: TJSXMLHTTPRequest;
  RequestData: Boolean;
  i: Integer;
  count: Integer;
  RepoJSON: TJSONArray;
  RepoShort: String;
begin

  // If the token doesn't pass muster then ask again
  if Length(GitHubToken) < 50 then
  begin
    WebEdit1.Text := '';
    WebEdit1.TextHint := 'Token is too short. Please try again.';
    Table_Data := '';
    exit;
  end;

  // Assume we're going to need to go and get data
  RequestData := True;

  // Get data from localStorage if it is avaialble
  if TWebLocalStorage.GetValue('GAE.TableData') <> '' then
  begin
    if TWEbLocalStorage.GetValue('GAE.TableDataAge') <> '' then
    begin
      Table_Data_Age := StrToFloat(TWebLocalStorage.GetValue('GAE.TableDataAge'));
      if MinutesBetween(Now, Table_Data_Age) < 60  then
      begin
//        console.log('Retrieving Table Data from Cache');
        Table_Data := TWebLocalStorage.GetValue('GAE.TableData');
        RequestData := False;
      end;
    end;
  end;


  if RequestData then
  begin
//    console.log('Retrieving Table Data From GitHub');
    WebEdit1.Text := '';
    WebEdit1.TextHint := 'Retrieving Repositories. Please Wait.';

    WebRequest := TWebHTTPRequest.Create(Self);
    WebRequest.URL := 'https://api.github.com/user/repos?per_page=50';
    WebRequest.Headers.AddPair('Accept','application/vnd.github+json');
    WebRequest.Headers.AddPair('Authorization','Bearer '+Form1.GitHubToken);
    WebResponse := await(TJSXMLHTTPRequest, WebRequest.Perform());
    Table_Data := String(WebResponse.Response);

    try
      TWebLocalStorage.SetValue('GAE.TableData',Table_Data);
      TWebLocalStorage.SetValue('GAE.TableDataAge',FloatToStr(Now));
    except on E: Exception do
      begin
        WebEdit1.Text := '';
        WebEdit1.TextHint := 'Retrieval Failed. Please try again.';
      end;
    end;

  end;

  // Doh!  API call to get repo info doesn't contain subscribers_count which IS included
  // when the usual public api call is made.  How infuriating.  So let's go get it again.
  if not(Assigned(WebRequest)) then WebRequest := TWebHTTPRequest.Create(Self);
  RepoJSON := TJSONObject.ParseJSONValue(Table_Data) as TJSONArray;
  i := 0;
  while i < RepoJSON.Count do
  begin
    count := -1;
    if (((RepoJSON[i] as TJSONObject).getValue('private')) as TJSONValue).toString = 'false' then
    begin
      WebRequest.URL := 'https://api.github.com/repos/'+((RepoJSON[i] as TJSONObject).getValue('full_name') as TJSONString).Value;
      WebResponse := await(TJSXMLHTTPRequest, WebRequest.Perform());
      if ((TJSONObject.ParseJSONValue(String(WebResponse.Response)) as TJSONObject).getValue('subscribers_count') as TJSONString) <> nil
      then count := ((TJSONObject.ParseJSONValue(String(WebResponse.Response)) as TJSONObject).getValue('subscribers_count') as TJSONNumber).asInt
      else count := -1;
    end;
    if count > -1
    then (RepoJSON[i] as TJSONObject).addPair('subscribers_count', count)
    else (RepoJSON[i] as TJSONObject).addPair('subscribers_count', 0);

    // Set pageviews to zero - will be updated later
    (RepoJSON[i] as TJSONObject).addPair('pageviews', 0);

    // Add Code name to data - Capital letters from last section of repository name
    RepoShort := ((RepoJSON[i] as TJSONObject).getValue('name') as TJSONString).Value;
    asm RepoShort = RepoShort.split('-').pop().replace(/[^A-Z]+/g, ""); end;
    (RepoJSON[i] as TJSONObject).addPair('short_name', RepoShort);

    i := i + 1;
  end;
  Table_Data := RepoJSON.ToString;

end;

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  CurrentCalendar := -1;

  tabReposBuilt := False;
  asm
    var headerMenu = [
      {
        label:"Select All",
        action:function(e, row){
          var t = row.getTable();
          pas.Unit1.Form1.PauseChartUpdates = true;
          for (var i = 1; i <= t.getDataCount(); i++) {
            t.getRowFromPosition(i).select();
          }
          pas.Unit1.Form1.PauseChartUpdates = false;
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
      initialSort:[
        {column:"subscribers_count", dir:"desc"}
      ],
      columns: [
        { title: "Repository", field: "name", bottomCalc: "count", widthGrow: 3, headerMenu: headerMenu,
            formatter: function(cell, formatterParams, OnRendered){
              return '<a href='+cell.getRow().getCell('html_url').getValue()+' target="_blank" style="text-decoration: none; color: black;">'+cell.getValue()+'</a>';
            }
        },
        { title: "API URL", field: "url", visible: false },
        { title: "URL", field: "html_url", visible: false },
        { title: "Full Name", field: "full_name", visible: false },
        { title: "Code", field: "short_name", visible: true },
        { title: "Updated", field: "pushed_at", widthGrow: 2, formatter: "datetime",
            formatterParams: {
              inputFormat:"iso",
              outputFormat:"yyyy-MMM-dd (ccc) HH:mm:ss"
            }
        },
        { title: "License", field: "license.name", widthGrow: 2 },
        { title: "Language", field: "language", widthGrow: 2 },
        { title: "Private", field: "private", formatter: "tickCross", bottomCalc: "sum" },
        { title: "Forks", field: "forks", bottomCalc: "sum", formatter:"money", formatterParams:{precision:false}, hozAlign:"right", sorter:"number" },
        { title: "Issues", field: "open_issues_count", bottomCalc: "sum", formatter:"money", formatterParams:{precision:false}, hozAlign:"right", sorter:"number" },
        { title: "Watchers", field: "subscribers_count", bottomCalc: "sum", formatter:"money", formatterParams:{precision:false}, hozAlign:"right", sorter:"number" },
        { title: "Stars", field: "stargazers_count", bottomCalc: "sum", formatter:"money", formatterParams:{precision:false}, hozAlign:"right", sorter:"number" },
        { title: "Views", field: "pageviews", bottomCalc: "sum", formatter:"money", formatterParams:{precision:false}, hozAlign:"right", sorter:"number" }
      ]
    });
    this.tabRepos.on("tableBuilt", function(){
      pas.Unit1.Form1.tabReposBuilt = true;
//      if ((pas.Unit1.Form1.automate == true) && (pas.Unit1.Form1.ParamCalendar == false)) {
//        pas.Unit1.Form1.RefreshTableData();
//      }
    });
    this.tabRepos.on("rowSelectionChanged", function(data, rows){
      //rows - array of row components for the selected rows in order of selection
      //data - array of data objects for the selected rows in order of selection
      pas.Unit1.Form1.UpdateChart();
    });
  end;


  // Parameters

  PauseChartUpdates := True;
  Highlight := '[none]';

  Param_Mode := 'UI';
  Param_GitHubToken := '';
  Param_Calendar := '';

  Param_Top := 0;
  Param_Left := 0;
  Param_Width := Form1.Width;
  Param_Height := (Form1.Height div 2);

  Param_FontSize := 10;
  Param_Background := '#212529';
  Param_Scale := '1';


  // See if they're already in localStorage

  Param_Mode := TWebLocalStorage.GetValue('GAE.M');
  Param_GitHubToken := TWebLocalStorage.GetValue('GAE.G');
  Param_Calendar := TWebLocalStorage.GetValue('GAE.C');

  Param_Top := StrToIntDef(TWebLocalStorage.GetValue('GAE.T'), Param_Top);
  Param_Left := StrToIntDef(TWebLocalStorage.GetValue('GAE.L'), Param_Left);
  Param_Width := StrToIntDef(TWebLocalStorage.GetValue('GAE.W'), Param_Width);
  Param_Height := StrToIntDef(TWebLocalStorage.GetValue('GAE.H'), Param_Height);

  Param_FontSize := StrToIntDef(TWebLocalStorage.GetValue('GAE.F'), Param_FontSize);
  Param_Background := TWebLocalStorage.GetValue('GAE.B');
  Param_Scale := TWebLocalStorage.GetValue('GAE.S');


  // Load up any that are passed as URL parameters
  if GetQueryParam('M') <> '' then Param_Mode := GetQueryParam('M');
  if GetQueryParam('G') <> '' then Param_GitHubToken := GetQueryParam('G');
  if GetQueryParam('C') <> '' then Param_Calendar := GetQueryParam('C');

  if GetQueryParam('T') <> '' then Param_Top := StrToIntDef(GetQueryParam('T'), Param_Top);
  if GetQueryParam('L') <> '' then Param_Left := StrToIntDef(GetQueryParam('L'), Param_Left);
  if GetQueryParam('W') <> '' then Param_Width := StrToIntDef(GetQueryParam('W'), Param_Width);
  if GetQueryParam('H') <> '' then Param_Height := StrToIntDef(GetQueryParam('H'), Param_Height);

  if GetQueryParam('F') <> '' then   Param_FontSize := StrToIntDef(GetQueryParam('F'), Param_FontSize);
  if GetQueryParam('B') <> '' then   Param_Background := GetQueryParam('B');
  if GetQueryParam('S') <> '' then   Param_Scale := GetQueryParam('S');


  // Use token if we've got one
  WebEdit1.Text := Param_GitHubToken;
  GitHubToken := Param_GitHubToken;


  // How our page is configured normally
  Form1.ElementClassName := 'vw-100 vh-100 d-flex p-2 bg-black';
  divMain.ElementClassName := 'd-flex flex-fill rounded border border-secondary border-2 p-2 flex-column overflow-hidden gap-1';
  divTabulator.ElementClassName := 'flex-fill d-none h-100 order-0';
  divChart.ElementClassName := 'position-relative overflow-hidden d-none flex-fill h-50 w-100 order-1';

  if Param_Mode = 'Calendar' then
  begin
    WebEdit1.Visible := False;
    divTabulator.Visible := False;
    Form1.ElementClassName := '';
    divMain.ElementClassName := '';
    divChart.ElementClassName := 'overflow-hidden order-1 calendar';
    UpdateCalendar;
  end
  else if Param_Mode = 'Chart' then
  begin
    document.documentElement.setAttribute('style','overflow:hidden;');
    WebEdit1.Visible := False;
    divTabulator.Visible := False;
    Form1.ElementClassName := '';
    divMain.ElementClassName := '';
    divChart.ElementClassName := 'overflow-hidden order-1';
    await(RefreshTableData);
    await(UpdateTable);
    UpdateChart;
  end
  else
  begin
    WebEdit1.Visible := True;
    divTabulator.Visible := False;
  end;


end;

procedure TForm1.WebFormResize(Sender: TObject);
begin
//  UpdateChart();
end;

procedure TForm1.WebFormShow(Sender: TObject);
begin
  WebEdit1.SetFocus;
end;

procedure TForm1.WebTimer1Timer(Sender: TObject);
begin
  if Param_Mode = 'Calendar' then
  begin
    UpdateCalendar;
  end
  else if Param_Mode = 'Chart' then
  begin
    await(RefreshTableData);
    await(UpdateTable);
    UpdateChart;
  end;
end;

end.