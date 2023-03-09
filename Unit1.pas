unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls, WebLib.JSON, jsdelphisystem,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, WEBLib.WebTools,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.REST, WEBLib.ExtCtrls, WEBLib.Storage, System.DateUtils;

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
  private
    { Private declarations }
  public
    { Public declarations }

    tabRepos: JSValue;
    tabReposBuilt: Boolean;

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
      if MinutesBetween(Now, RepoAge) < 60 then
      begin
        RequestData := False;
        Result := TWebLocalStorage.getValue('GAE.Repo.Data.'+Repo);
        console.log('Retrieving ['+Repo+'] Data from Cache');
      end;
    end;
  end;

  // Otherwise, go and get it
  if RequestData then
  begin
    console.log('Retrieving ['+Repo+'] Data from GitHub');
    WebRequest := TWebHTTPRequest.Create(Self);
    WebRequest.URL := 'https://api.github.com/repos/'+repo+'/traffic/views';
    WebRequest.Headers.AddPair('Accept','application/vnd.github+json');
    WebRequest.Headers.AddPair('Authorization','Bearer '+GitHubToken);
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

  asm
      async function sleep(msecs) {
        return new Promise((resolve) =>setTimeout(resolve, msecs));
      }
      await sleep(100);

    document.body.style.setProperty('background', this.Param_Background);
    GitHubCalendar(".calendar", this.Param_Calendar, { responsive: true });
  end;

  divChart.Visible := True;
end;

procedure TForm1.UpdateChart;
var
  NumRepos: Integer;
begin

  while not(tabReposBuilt) do
  begin
    asm
      async function sleep(msecs) {
        return new Promise((resolve) =>setTimeout(resolve, msecs));
      }
      await sleep(100);
    end;
  end;

  console.log('drawing chart');

  NumRepos := 0;

  divChart.Top := Param_Top;
  divChart.Left := Param_Left;
  divChart.Width := Param_Width;
  divChart.Height := Param_Height;

  asm
    document.body.style.setProperty('background', this.Param_Background);

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
          console.log('Removing empty starting date: '+ChartData[i].date);
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
          console.log('Removing empty ending date: '+ChartData[i].date);
          ChartData.splice(i,1);
          i = i - 1;
        }
      }

//      for (var i = 0; i < ChartData.length; i++) {
//        var list = ChartData[i];
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

        var margin = 8;
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
        var x = d3.scaleLinear().domain([0,ChartData.length-0.5]).range([margin*5,width]);
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
        var y = d3.scaleLinear().domain([0, yMax]).range([height - ParamY, 0])
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
               .attr('x', (d,i) => x(j) - (width/ChartData.length/2))
               .attr('y', d => y(d[1]))
               .attr('width', (width/ChartData.length))
               .attr('height', d => {
                 return y(d[0])-y(d[1])
               })
               .attr('fill', "#F00")
               .attr('stroke', 'black')
               .attr('stroke-width', 1)
               .append("title")
               .text(function(d,i) {
                 return d.key+' - '+d.data[d.key]
               });
             d3.select(this)
               .append("text")
               .attr('x', (d,i) => x(j))
               .attr('y', d => y(d[1])+(y(d[0])-y(d[1]))/2+(parseInt(pas.Unit1.Form1.Param_FontSize) / 3))
               .attr('width', (width/ChartData.length))
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
    if table_data = '' then console.log('waiting for table');
    if not(tabReposBuilt) then console.log('waiting for table');

    asm
      async function sleep(msecs) {
        return new Promise((resolve) =>setTimeout(resolve, msecs));
      }
      await sleep(100);
    end;
  end;


  asm
    this.tabRepos.setData(JSON.parse(this.Table_Data));
    this.tabRepos.selectRow();
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
        console.log('Retrieving Table Data from Cache');
        Table_Data := TWebLocalStorage.GetValue('GAE.TableData');
        RequestData := False;
      end;
    end;
  end;


  if RequestData then
  begin
    console.log('Retrieving Table Data From GitHub');
    WebEdit1.Text := '';
    WebEdit1.TextHint := 'Retrieving Repositories. Please Wait.';

    WebRequest := TWebHTTPRequest.Create(Self);
    WebRequest.URL := 'https://api.github.com/user/repos';
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

  Param_Mode := 'UI';
  Param_GitHubToken := '';
  Param_Calendar := '';

  Param_Top := 0;
  Param_Left := 0;
  Param_Width := Form1.Width;
  Param_Height := (Form1.Height div 2);

  Param_FontSize := 10;
  Param_Background := '#212529';


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