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
    [async] procedure UpdateWebTraffic;
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
    Param_Account: String;
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
    await sleep(100);
    pas.Unit1.Form1.NextCalendar = new Date().getUTCDay()
  } end;
  {$ENDIF}

  if CurrentCalendar <> NextCalendar then
  begin
    if CurrentCalendar = -1  then
    begin
      {$IFNDEF WIN32}
      asm {
//        divChart.addEventListener('click', () =>
//          window.open(window.location.href, '_blank').focus()
//        );
      } end;
      {$ENDIF}
    end;

    CurrentCalendar := NextCalendar;

    {$IFNDEF WIN32}
    asm {

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

  const options = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: `bearer ${GITHUB_TOKEN}`
    },
    body: JSON.stringify({ query: QUERY })
  };

  return fetch('https://api.github.com/graphql', options)
    .then(res => res.json())
    .then(data => {
      let contributions = [];
      data.data.user.contributionsCollection.contributionCalendar.weeks.forEach((week, weekIndex) => {
        week.contributionDays.forEach(day => {
          contributions.push({
            date: day.date,
            count: day.contributionCount,
            weekNumber: weekIndex + 1
          });
        });
      });
      return contributions;
    })
    .catch(err => {
      console.log(err);
    });
}

var calendardata = await Get_GitHub_Data(
  this.Param_Calendar,
  this.Param_GitHubToken,
  new Date(new Date().setFullYear(new Date().getFullYear() - 1)).toJSON(),
  new Date().toJSON()
);

var divChart = document.getElementById('divChart');
divChart.replaceChildren();

var started = false;
var totalContributions = 0;
var daysWithContributions = 0;
var busiestDay = { date: '', count: 0 };
var currentStreak = 0;
var longestStreak = 0;
var busiestWeek = 0;
var busiestWeekContributions = 0;

// Calculate weekday counts
const weekdayCounts = new Array(7).fill(0);
calendardata.forEach(day => {
  const weekday = new Date(day.date).getDay();
  weekdayCounts[weekday] += day.count;
});
const mostActiveDayIndex = weekdayCounts.indexOf(Math.max(...weekdayCounts));
const mostActiveDay = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][mostActiveDayIndex];

// Calculate longest streak
let currentStreakLength = 0;
calendardata.forEach(day => {
  if (day.count > 0) {
    currentStreakLength++;
    longestStreak = Math.max(longestStreak, currentStreakLength);
  } else {
    currentStreakLength = 0;
  }
});

// Calculate busiest week
calendardata.forEach(day => {
  if (day.count > busiestWeekContributions) {
    busiestWeek = day.weekNumber;
    busiestWeekContributions = day.count;
  }
});

for (let i = 0; i < calendardata.length; i++) {
  if ((started == false) && (new Date(calendardata[i].date).getUTCDay() == 0)) {
    started = true;
  }

  if (started == true) {
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

    cal.setAttribute('title', `Week ${calendardata[i].weekNumber}: ${calendardata[i].date} - ${calendardata[i].count} contributions`);
    divChart.appendChild(cal);

    // Update summary values
    totalContributions += calendardata[i].count;
    if (calendardata[i].count > 0) {
      daysWithContributions++;
      currentStreak++;
    } else {
      currentStreak = 0;
    }
    if (calendardata[i].count > busiestDay.count) {
      busiestDay = { date: calendardata[i].date, count: calendardata[i].count };
    }
  }
}

// Calculate and display summary information
const totalDays = calendardata.length;
const dutyCycle = ((daysWithContributions / totalDays) * 100).toFixed(1);
const busiestDayStr = `${new Date(busiestDay.date).toLocaleDateString('en-US', { month: 'short', day: '2-digit' })} (${busiestDay.count}c)`;
const busiestWeekdayStr = `${mostActiveDay} (${weekdayCounts[mostActiveDayIndex]}c)`;
const longestStreakStr = `${longestStreak}`;
const busiestWeekStr = `W${busiestWeek}`;

const summaryElement = document.createElement('div');
summaryElement.id = 'calendar-summary';
summaryElement.innerHTML = `
  <div class="calendar-summary">
    <div class="calendar-label">C:</div><div class="calendar-value">${totalContributions}</div>
    <div class="calendar-label">D:</div><div class="calendar-value">${dutyCycle}%</div>
    <div class="calendar-label">Day:</div><div class="calendar-value">${busiestDayStr}</div>
    <div class="calendar-label">Wd:</div><div class="calendar-value">${busiestWeekdayStr}</div>
    <div class="calendar-label">Wk:</div><div class="calendar-value">${busiestWeekStr}</div>
    <div class="calendar-label">Streak:</div><div class="calendar-value">${currentStreak}d</div>
    <div class="calendar-label">Record:</div><div class="calendar-value">${longestStreakStr}d</div>
  </div>
`;

divChart.parentNode.appendChild(summaryElement);
    }  end;
    {$ENDIF}
  end;

  divChart.Visible := True;
end;

procedure TForm1.UpdateChart;
begin
  divChart.Top := Param_Top;
  divChart.Left := Param_Left;
  divChart.Width := Param_Width;
  divChart.Height := Param_Height;

  {$IFNDEF WIN32}
  asm {
    const pageSize = 50;

    await sleep(100);

    divMain.style.setProperty('background-color', this.Param_Background,'important');
    divMain.style.setProperty('position', 'absolute');
    divMain.style.setProperty('width', '100%');
    divMain.style.setProperty('height', '100%');

    divChart.classList.replace('d-none','d-flex');
    divChart.replaceChildren();


    const addOneDayToDate = (date) => {
      date.setDate(date.getDate() + 1)
      return date
    }


    const fetchGitHubRepoData = async (githubToken, pageSize, refresh = false) => {
      const headers = {
        Authorization: `Bearer ${githubToken}`,
        Accept: 'application/vnd.github+json',
      };

      let page = 1;
      let repos = [];
      let hasMorePages = true;

      while (hasMorePages) {
        const response = await fetch(`https://api.github.com/user/repos?per_page=${pageSize}&page=${page}`, {
          headers,
        });
        const repoData = await response.json();
        repos = [...repos, ...repoData];
        hasMorePages = repoData.length === pageSize;
        page++;
      }

      for(var i = 0; i < repos.length; i++) {
        if (!repos[i].full_name.startsWith(this.Param_Account)) {
          repos.splice(i,1);
        }
      }

      const updatedRepos = await Promise.all(
        repos.map(async (repo) => {
          const [trafficData, discussionsData] = await Promise.all([
            fetch(`https://api.github.com/repos/${repo.owner.login}/${repo.name}/traffic/views`, {
              headers,
            }).then(res => res.json()),
            fetch('https://api.github.com/graphql', {
              method: 'POST',
              headers: {
                ...headers,
                'Content-Type': 'application/json',
            },
              body: JSON.stringify({
                    query: `
                  query($owner: String!, $name: String!) {
                    repository(owner: $owner, name: $name) {
                      discussions {
                       totalCount
                      }
                    }
              }
                `,
                variables: {
                 owner: repo.owner.login,
                  name: repo.name,
                },
              }),
            }).then(res => res.json())
           ]);

          return {
            ...repo,
            traffic: {
              views: trafficData.views.map(view => ({
                timestamp: addOneDayToDate(new Date(view.timestamp)),
                uniques: view.uniques,
              })),
            },
            updated_at: repo.updated_at,
            language: repo.language,
            license: repo.license ? repo.license.name : null,
            private: repo.private,
            stargazers_count: repo.stargazers_count || 0,
            forks_count: repo.forks_count || 0,
            watchers_count: repo.watchers_count || 0,
            discussions_count: discussionsData.data.repository.discussions.totalCount,
            subscribers_count: repo.subscribers_count || 0,
          }
        })
      )
      return updatedRepos;
    }


    const processTrafficData = (repos) => {
      const processedData = repos.map((repo) => {
        const { name, full_name, traffic } = repo;
        let shortName = name.replace(/[^A-Z]/g, '');
        shortName = shortName.replace(/^TMSWEBC/, '').replace(/^TMSXD/, '');
        const dates = traffic.views.map((view) => new Date(view.timestamp));
        const totalViews = traffic.views.reduce(
          (total, view) => total + view.uniques,
          0
        );

        return {
          name,
          full_name,
          shortName,
          totalViews,
          minDate: new Date(Math.min(...dates)),
          maxDate: new Date(Math.max(...dates)),
          updated_at: repo.updated_at,
          language: repo.language,
          license: repo.license,
          private: repo.private,
          stargazers_count: repo.stargazers_count || 0,
          forks_count: repo.forks_count || 0,
          watchers_count: repo.watchers_count || 0,
          discussions_count: repo.discussions_count || 0,
          subscribers_count: repo.subscribers_count || 0,
          traffic: traffic.views,
        };
      });


      const sortedData = processedData.sort((a, b) => {
        if (b.totalViews === a.totalViews) {
          if (a.shortName === b.shortName) {
            return a.full_name.localeCompare(b.full_name);
          }
          return a.shortName.localeCompare(b.shortName);
        }
        return b.totalViews - a.totalViews;
      });

      return sortedData;
    };


    const getTrafficData = (githubToken, pageSize, refresh = false) => {
      return new Promise((resolve, reject) => {
        const request = window.indexedDB.open('trafficData', 1);

        request.onupgradeneeded = (event) => {
          const db = event.target.result;
          const store = db.createObjectStore('repos', { keyPath: 'full_name' });
          store.createIndex('name', 'name', { unique: false });
          store.createIndex('timestamp', 'timestamp', { unique: false });
          const metaStore = db.createObjectStore('meta', { keyPath: 'id' });
        };

        request.onsuccess = async (event) => {
          const db = event.target.result;
          const transaction = db.transaction(['meta'], 'readonly');
          const metaStore = transaction.objectStore('meta');
          const request = metaStore.get('lastUpdated');

          request.onsuccess = async () => {
            const lastUpdatedObj = request.result;
            const lastUpdatedTime = new Date(lastUpdatedObj ? lastUpdatedObj.timestamp : 0);
            const currentTime = new Date();

//            if (refresh || currentTime - lastUpdatedTime > 60 * 60 * 1000) {

            // Data needs to be refreshed or the hour (more specifically the day) has changed
            if (refresh || (currentTime.getHours() !== lastUpdatedTime.getHours())) {
              // Data needs to be refreshed or is older than 60 minutes
              const updatedRepos = await fetchGitHubRepoData(githubToken, pageSize, refresh);
              const processedData = processTrafficData(updatedRepos);
              const mergedData = await mergeWithCachedData(processedData, db);
              await storeTrafficData(mergedData, currentTime, db);
              resolve(mergedData);
            } else {
              // Data is fresh, read from IndexedDB
              const repoData = await readFromIndexedDB(db);
              resolve(repoData);
            }
          };
        };

        request.onerror = (event) => {
          reject(event.target.error);
        };
      });
    };


    const mergeWithCachedData = async (processedData, db) => {
      const transaction = db.transaction(['repos'], 'readonly');
      const store = transaction.objectStore('repos');

      const mergedData = await Promise.all(
        processedData.map(async (repo) => {
          const request = store.get(repo.full_name);
          const cachedRepo = await new Promise((resolve, reject) => {
            request.onsuccess = () => {
              resolve(request.result);
            };
            request.onerror = () => {
              reject(request.error);
            };
          });

          if (cachedRepo) {
            const mergedTraffic = repo.traffic.map((view) => {
              const cachedView = cachedRepo.traffic.find(
                (v) => v.timestamp === view.timestamp
              );
              return {
                ...view,
                uniques: Math.max(view.uniques, cachedView ? cachedView.uniques : 0),
              };
            });
            return {
              ...repo,
              traffic: mergedTraffic,
            };
          }
          return repo;
        })
      );

      return mergedData;
    };


    const storeTrafficData = async (processedData, timestamp, db) => {
      const transaction = db.transaction(['repos', 'meta'], 'readwrite');
      const store = transaction.objectStore('repos');
      const metaStore = transaction.objectStore('meta');

      await Promise.all(
        processedData.map(async (repo) => {
          await store.put({
            full_name: repo.full_name,
            shortName: repo.shortName,
            name: repo.name,
            traffic: repo.traffic,
            totalViews: repo.totalViews,
            minDate: repo.minDate,
            maxDate: repo.maxDate,
            language: repo.language,
            private: repo.private,
            discussions_count: repo.discussions_count,
            stargazers_count: repo.stargazers_count,
            watchers_count: repo.watchers_count,
            forks_count: repo.forks_count,
            subscribers_count: repo.subscribers_count,
            updated_at: repo.updated_at,
            license: repo.license,
          });
        })
      );

      await metaStore.put({ id: 'lastUpdated', timestamp });
    };


    const readFromIndexedDB = async (db) => {
      const transaction = db.transaction(['repos'], 'readonly');
      const store = transaction.objectStore('repos');

      const repoData = [];
      const request = store.openCursor();

      return new Promise((resolve, reject) => {
        request.onsuccess = () => {
          const cursor = request.result;
          if (cursor) {
            repoData.push(cursor.value);
            cursor.continue();
          } else {
            resolve(
              repoData.sort((a, b) => {
                if (b.totalViews !== a.totalViews) return b.totalViews - a.totalViews;
                if (a.shortName !== b.shortName) return a.shortName.localeCompare(b.shortName);
                return a.name.localeCompare(b.name);
              })
            );
          }
        };
        request.onerror = () => {
          reject(request.error);
        };
      });
    };


function chartTraffic(
  data,
  container,
  width,
  height,
  margin,
  colors,
  fonts = {
    family: "Arial, sans-serif",
    axis: "12px",
    repo: "12px",
    label: "14px",
    clip: "25px",
    percent: "14px"
  },
  offsets = {
    countOffset: "15px",
    pctLeft: "10px",
    pctTop: "10px"
  },
  rounding = "5px",
  animTime = 1500
) {
  // Parse and format dates
  const parseDate = d3.utcParse("%Y-%m-%dT%H:%M:%SZ");
  const formatDate = d3.timeFormat("%b%d");
  const longDate = d3.timeFormat("%Y-%b-%d (%a)");

  // Filter the data to include the most recent 14 days
  const today = new Date();
  const mostRecentDate = d3.max(data.flatMap(d => d.traffic), d => new Date(d.timestamp));
  const startDate = d3.timeDay.offset(mostRecentDate, -14);
  const filteredData = data.map(repo => ({
    ...repo,
    traffic: repo.traffic.filter(d => new Date(d.timestamp) >= startDate && new Date(d.timestamp) <= mostRecentDate)
  }));

  // Create an array of dates for the most recent 14 days
  // Seems Github does fun things with dates, so let's lop off the last one from the chart.
  var dates = d3.timeDays(startDate, d3.timeDay.offset(mostRecentDate, 1));
  dates.pop();

  // Create an object to store the total unique visits per date
  const totalVisitsPerDate = {};
  dates.forEach(date => {
    totalVisitsPerDate[formatDate(date)] = filteredData.reduce((total, repo) => {
      const visit = repo.traffic.find(t => formatDate(new Date(t.timestamp)) === formatDate(date));
      return total + (visit ? visit.uniques : 0);
    }, 0);
  });

  // Calculate the total unique visitors for each repository
  const repoTotals = filteredData.map(repo => ({
    full_name: repo.full_name,
    total: repo.traffic.reduce((sum, d) => sum + d.uniques, 0)
  }));

  // Assign colors to the repositories based on their total unique visitors
  const repoColors = repoTotals.map(repo => {
    if (repo.total <= 10) return colors.low;
    if (repo.total <= 20) return colors.med;
    return colors.high;
  });

  // Create a set to store the selected repositories
  const selectedRepos = new Set();

  container.replaceChildren();

  // Set up the SVG container
  const svg = d3.select(container)
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", `translate(${margin.left}, ${margin.top})`);

  // Set up the x-axis scale and axis
  const x = d3.scaleBand()
    .domain(dates)
    .range([0, width - margin.left - margin.right])
    .padding(0.1);

  const xAxis = d3.axisBottom(x)
    .tickFormat(d => formatDate(d));

  // Set up the y-axis scale and axis
  const y = d3.scaleLinear()
    .domain([0, d3.max(Object.values(totalVisitsPerDate))])
    .range([height - margin.top - margin.bottom, 0]);

  // Add a new object to store the total unique visits across all repositories
  const totalUniqueVisits = repoTotals.reduce((total, repo) => total + repo.total, 0);

  // Update the colors of the segments based on the selection
  function updateColors() {
    const selectedRepoTotals = [...selectedRepos].map(repoName => repoTotals.find(r => r.full_name === repoName));
    const totalSelectedVisits = selectedRepoTotals.reduce((total, repo) => total + repo.total, 0);
    const percentage = totalSelectedVisits / totalUniqueVisits * 100;

    svg.selectAll("rect")
      .attr("fill", function(d) {
        const repoName = d3.select(this.parentNode).datum().key;
        if (selectedRepos.has(repoName)) {
          return colors.selected;
        } else {
          const repoIndex = filteredData.findIndex(r => r.full_name === repoName);
          return repoColors[repoIndex];
        }
      });

    if (selectedRepos.size > 0) {
      percentageText.text(`${percentage.toFixed(1)}%`);
      uniqueText.text(`${totalSelectedVisits} of ${totalUniqueVisits}`);
    } else {
      percentageText.text("");
      uniqueText.text("");
    }
  }


  // Create the stacked data
  const stackedData = d3.stack()
    .keys(filteredData.map(d => d.full_name))
    .value((d, key) => {
      const repo = filteredData.find(r => r.full_name === key);
      const visit = repo.traffic.find(t => formatDate(new Date(t.timestamp)) === formatDate(d.data));
      return visit ? visit.uniques : 0;
    })
    (dates.map(date => ({ data: date })));

  // Draw the stacked bars
  svg.append("g")
    .selectAll("g")
    .data(stackedData)
    .join("g")
    .attr("fill", (d, i) => repoColors[i])
    .selectAll("rect")
    .data(d => d)
    .join("rect")
    .attr("x", d => x(d.data.data))
    .attr("y", height - margin.top - margin.bottom)
    .attr("height", 0)
    .attr("width", x.bandwidth())
    .attr("rx", rounding)
    .attr("ry", rounding)
    .attr("cursor", "pointer")
    .on("click", function(event, d) {
      const repoName = d3.select(this.parentNode).datum().key;
      if (selectedRepos.has(repoName)) {
        selectedRepos.delete(repoName);
      } else {
        selectedRepos.add(repoName);
      }
      updateColors();
    })
    .transition()
    .duration(animTime)
    .attr("y", d => y(d[1]) + 1)
    .attr("height", d => Math.max(y(d[0]) - y(d[1]) - 1, 0))
    .each(function(d) {
      const repoName = d3.select(this.parentNode).datum().key;
      const repo = filteredData.find(r => r.full_name === repoName);
      const visitCount = d[1] - d[0];
      const segmentHeight = Math.max(y(d[0]) - y(d[1]) - 1, 0);

      // Add tooltip
      d3.select(this)
        .append("title")
        .text(`${repo.name}\n${visitCount} Unique Visitor(s)\n${longDate(d.data.data)}`);

      // Add label if the segment height is greater than or equal to the clip threshold
      if (segmentHeight >= parseInt(fonts.clip)) {
        const label = d3.select(this.parentNode)
          .append("text")
          .attr("x", x(d.data.data) + x.bandwidth() / 2)
          .attr("y", height - margin.top - margin.bottom)
          .attr("text-anchor", "middle")
          .attr("dy", "0.35em")
          .style("fill", colors.label)
          .style("font-size", fonts.label)
          .style("font-family", fonts.family)
          .style("pointer-events", "none")
          .style("opacity", 0)
          .text(repo.shortName);

        label.transition()
          .duration(animTime)
          .attr("y", y(d[1]) + segmentHeight / 2 + 1)
          .style("opacity", 1);
      }
    });

  // Draw the x-axis
  svg.append("g")
    .attr("transform", `translate(0, ${height - margin.top - margin.bottom})`)
    .call(xAxis)
    .selectAll("text")
    .style("text-anchor", "middle")
    .style("fill", colors.axisText)
    .style("font-size", fonts.axis)
    .style("font-family", fonts.family);

  svg.selectAll(".tick line")
    .style("stroke", colors.axisLines);

  svg.select(".domain")
    .style("stroke", colors.axisLines);

  // Add total unique visits below each date
  svg.append("g")
    .attr("transform", `translate(0, ${height - margin.top - margin.bottom + parseInt(offsets.countOffset)})`)
    .selectAll("text")
    .data(dates)
    .join("text")
    .attr("x", d => x(d) + x.bandwidth() / 2)
    .attr("y", 0)
    .attr("text-anchor", "middle")
    .style("fill", colors.axisText)
    .style("font-size", fonts.axis)
    .style("font-family", fonts.family)
    .text(d => totalVisitsPerDate[formatDate(d)]);

  // Create a selection to hold the percentage text element
  let percentageText = svg.append("text")
    .attr("x", parseInt(offsets.percentLeft))
    .attr("y", parseInt(offsets.percentTop))
    .style("font-size", fonts.percent)
    .style("font-family", fonts.family)
    .style("fill", colors.percent)
    .style("pointer-events", "none")
    .style("text-shadow", "0px 0px 10px rgba(255, 255, 255, 1)")
    .text("");

  // Create a selection to hold the unique vistits text element
  let uniqueText = svg.append("text")
    .attr("x", width - 3*parseInt(offsets.percentLeft))
    .attr("y", parseInt(offsets.percentTop))
    .style("font-size", fonts.percent)
    .style("font-family", fonts.family)
    .style("fill", colors.percent)
    .style("text-anchor", "end")
    .style("pointer-events", "none")
    .style("text-shadow", "0px 0px 10px rgba(255, 255, 255, 1)")
    .text("");

//      return {
//        svg,
//        x,
//        xAxis,
//        y
//      };
    }


    getTrafficData(this.Param_GitHubToken, pageSize, true)
      .then(processedData => {

        // Use the processedData for further processing or rendering
        const container = document.getElementById("divChart");
        const width = this.Param_Width;
        const height = this.Param_Height;
        const margin = { top: 0, right: 10, bottom: 50, left: 10 };
        const colors = {
          bg: this.Param_Background,
          high: "#ff0000", // Color for repositories with high popularity
          med: "#800000", // Color for repositories with medium popularity
          low: "#400000", // Color for repositories with low popularity
          selected: "green", // Color for selected repositories
          axisText: "#999999", // Color for axis labels and total unique visits
          axisLines: "#cccccc", // Color for axis lines
          label: "white", // Color for segment labels
          percent: "white" // color of percent indicator
        };
        const fonts = {
          family: "Cairo, sans-serif", // Font family for all text elements
          axis: "10px", // Font size for axis labels
          repo: "10px", // Font size for repository names (unused)
          label: "10px", // Font size for segment labels
          clip: "20px", // Minimum height threshold for displaying segment labels
          percent: "24px" // size of percent indicator
        };
        const offsets = {
          countOffset: "30px", // Vertical offset between date labels and total unique visits
          percentLeft: "10px",
          percentTop: "30px"
        };
        const rounding = "5px"; // Amount of rounding for bar segments
        const animTime = 1500; // Duration of the animation in milliseconds

        chartTraffic(
          processedData,
          container,
          width,
          height,
          margin,
          colors,
          fonts,
          offsets,
          rounding,
          animTime
        );
      })
      .catch(error => {
        console.error('Error fetching traffic data:', error);
      });

  } end;
  {$ENDIF}

  divChart.Visible := True;

//  if (NumRepos = 0) then
//  begin
//    divChart.ElementHandle.classList.add('d-none');
//    divTabulator.ElementHandle.classList.replace('h-50','h-100');
//  end
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


procedure TForm1.UpdateWebTraffic;
begin
  divChart.Top := Param_Top;
  divChart.Left := Param_Left;
  divChart.Width := Param_Width;
  divChart.Height := Param_Height;

  {$IFNDEF WIN32}
  asm {
    await sleep(100);

    divMain.style.setProperty('background-color', this.Param_Background,'important');
    divMain.style.setProperty('position', 'absolute');
    divMain.style.setProperty('width', '100%');
    divMain.style.setProperty('height', '100%');

    divChart.classList.replace('d-none','d-flex');
    divChart.replaceChildren();

   async function fetchWebData(url) {
      try {
        const response = await fetch(url);
        const data = await response.json();
        return data;
      } catch (error) {
        console.error('Error fetching data:', error);
        throw error;
      }
    }

function processWebData(jsonData) {
  const processedData = [];

  for (const website in jsonData) {
    const { title, shortname, visitors } = jsonData[website];
    const trafficData = Object.entries(visitors).map(([date, uniques]) => ({
      timestamp: new Date(`${date}T00:00:00Z`).toISOString(),
      uniques
    }));

    processedData.push({
      full_name: title,
      name: title,
      shortName: shortname,
      traffic: trafficData
    });
  }

  // Sort the repositories based on total unique visitors in descending order
  processedData.sort((a, b) => {
    const totalA = a.traffic.reduce((sum, entry) => sum + entry.uniques, 0);
    const totalB = b.traffic.reduce((sum, entry) => sum + entry.uniques, 0);
    return totalB - totalA;
  });

  return processedData;
}

    function chartWebTraffic(
      data,
      container,
      width,
      height,
      margin,
      colors,
      fonts = {
        family: "Arial, sans-serif",
        axis: "12px",
        repo: "12px",
        label: "14px",
        clip: "25px",
        percent: "14px"
      },
      offsets = {
        countOffset: "15px",
        pctLeft: "10px",
        pctTop: "10px"
      },
      rounding = "5px",
      animTime = 1500
    ) {
      // Parse and format dates
      const parseDate = d3.utcParse("%Y-%m-%dT%H:%M:%SZ");
      const formatDate = d3.utcFormat("%b%d");
      const longDate = d3.utcFormat("%Y-%b-%d (%a)");

      // Filter the data to include the most recent 14 days
      const today = new Date();
      const mostRecentDate = d3.max(data.flatMap(d => d.traffic), d => new Date(d.timestamp));
      const startDate = d3.timeDay.offset(mostRecentDate, -13);
      const filteredData = data.map(repo => ({
        ...repo,
        traffic: repo.traffic.filter(d => new Date(d.timestamp) >= startDate && new Date(d.timestamp) <= mostRecentDate)
      }));

      // Create an array of dates for the most recent 14 days
      // Seems Github does fun things with dates, so let's lop off the last one from the chart.
      var dates = d3.timeDays(startDate, d3.timeDay.offset(mostRecentDate, 2));
      dates.pop();

      // Create an object to store the total unique visits per date
      const totalVisitsPerDate = {};
      dates.forEach(date => {
        totalVisitsPerDate[formatDate(date)] = filteredData.reduce((total, repo) => {
          const visit = repo.traffic.find(t => formatDate(new Date(t.timestamp)) === formatDate(date));
          return total + (visit ? visit.uniques : 0);
        }, 0);
      });

      // Calculate the total unique visitors for each repository
      const repoTotals = filteredData.map(repo => ({
        full_name: repo.full_name,
        total: repo.traffic.reduce((sum, d) => sum + d.uniques, 0)
      }));

      // Assign colors to the repositories based on their total unique visitors
      const repoColors = repoTotals.map(repo => {
        if (repo.total <= 50) return colors.low;
        if (repo.total <= 250) return colors.med;
        return colors.high;
      });

      // Create a set to store the selected repositories
      const selectedRepos = new Set();

      container.replaceChildren();

      // Set up the SVG container
      const svg = d3.select(container)
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", `translate(${margin.left}, ${margin.top})`);

      // Set up the x-axis scale and axis
      const x = d3.scaleBand()
        .domain(dates)
        .range([0, width - margin.left - margin.right])
        .padding(0.1);

      const xAxis = d3.axisBottom(x)
        .tickFormat(d => formatDate(d));

      // Set up the y-axis scale and axis
      const y = d3.scaleLinear()
        .domain([0, d3.max(Object.values(totalVisitsPerDate))])
        .range([height - margin.top - margin.bottom, 0]);

      // Add a new object to store the total unique visits across all repositories
      const totalUniqueVisits = repoTotals.reduce((total, repo) => total + repo.total, 0);

      // Update the colors of the segments based on the selection
      function updateColors() {
        const selectedRepoTotals = [...selectedRepos].map(repoName => repoTotals.find(r => r.full_name === repoName));
        const totalSelectedVisits = selectedRepoTotals.reduce((total, repo) => total + repo.total, 0);
        const percentage = totalSelectedVisits / totalUniqueVisits * 100;

        svg.selectAll("rect")
          .attr("fill", function(d) {
            const repoName = d3.select(this.parentNode).datum().key;
            if (selectedRepos.has(repoName)) {
              return colors.selected;
            } else {
              const repoIndex = filteredData.findIndex(r => r.full_name === repoName);
              return repoColors[repoIndex];
            }
          });

        if (selectedRepos.size > 0) {
          percentageText.text(`${percentage.toFixed(1)}%`);
          uniqueText.text(`${totalSelectedVisits} of ${totalUniqueVisits}`);
        } else {
          percentageText.text("");
          uniqueText.text("");
        }
      }

      // Create the stacked data
      const stackedData = d3.stack()
        .keys(filteredData.map(d => d.full_name))
        .value((d, key) => {
          const repo = filteredData.find(r => r.full_name === key);
          const visit = repo.traffic.find(t => formatDate(new Date(t.timestamp)) === formatDate(d.data));
          return visit ? visit.uniques : 0;
        })
        (dates.map(date => ({ data: date })));


      // Draw the stacked bars
      svg.append("g")
        .selectAll("g")
        .data(stackedData)
        .join("g")
        .attr("fill", (d, i) => repoColors[i])
        .selectAll("rect")
        .data(d => d)
        .join("rect")
        .attr("x", d => x(d.data.data))
        .attr("y", height - margin.top - margin.bottom)
        .attr("height", 0)
        .attr("width", x.bandwidth())
        .attr("rx", rounding)
        .attr("ry", rounding)
        .attr("cursor", "pointer")
        .on("click", function(event, d) {
          const repoName = d3.select(this.parentNode).datum().key;
          if (selectedRepos.has(repoName)) {
            selectedRepos.delete(repoName);
          } else {
            selectedRepos.add(repoName);
          }
          updateColors();
        })
        .transition()
        .duration(animTime)
        .attr("y", d => y(d[1]) + 1)
        .attr("height", d => Math.max(y(d[0]) - y(d[1]) - 1, 0))
        .each(function(d) {
          const repoName = d3.select(this.parentNode).datum().key;
          const repo = filteredData.find(r => r.full_name === repoName);
          const visitCount = d[1] - d[0];
          const segmentHeight = Math.max(y(d[0]) - y(d[1]) - 1, 0);

          // Add tooltip
          d3.select(this)
            .append("title")
            .text(`${repo.name}\n${visitCount} Unique Visitor(s)\n${longDate(d.data.data)}`);

          // Add label if the segment height is greater than or equal to the clip threshold
          if (segmentHeight >= parseInt(fonts.clip)) {
            const label = d3.select(this.parentNode)
              .append("text")
              .attr("x", x(d.data.data) + x.bandwidth() / 2)
              .attr("y", height - margin.top - margin.bottom)
              .attr("text-anchor", "middle")
              .attr("dy", "0.35em")
              .style("fill", colors.label)
              .style("font-size", fonts.label)
              .style("font-family", fonts.family)
              .style("pointer-events", "none")
              .style("opacity", 0)
              .text(repo.shortName);

            label.transition()
              .duration(animTime)
              .attr("y", y(d[1]) + segmentHeight / 2 + 1)
              .style("opacity", 1);
          }
        });

      // Draw the x-axis
      svg.append("g")
        .attr("transform", `translate(0, ${height - margin.top - margin.bottom})`)
        .call(xAxis)
        .selectAll("text")
        .style("text-anchor", "middle")
        .style("fill", colors.axisText)
        .style("font-size", fonts.axis)
        .style("font-family", fonts.family);

      svg.selectAll(".tick line")
        .style("stroke", colors.axisLines);

      svg.select(".domain")
        .style("stroke", colors.axisLines);

      // Add total unique visits below each date
      svg.append("g")
        .attr("transform", `translate(0, ${height - margin.top - margin.bottom + parseInt(offsets.countOffset)})`)
        .selectAll("text")
        .data(dates)
        .join("text")
        .attr("x", d => x(d) + x.bandwidth() / 2)
        .attr("y", 0)
        .attr("text-anchor", "middle")
        .style("fill", colors.axisText)
        .style("font-size", fonts.axis)
        .style("font-family", fonts.family)
        .text(d => totalVisitsPerDate[formatDate(d)]);

      // Create a selection to hold the percentage text element
      let percentageText = svg.append("text")
        .attr("x", parseInt(offsets.pctLeft))
        .attr("y", parseInt(offsets.pctTop))
        .style("font-size", fonts.percent)
        .style("font-family", fonts.family)
        .style("fill", colors.percent)
        .style("pointer-events", "none")
        .style("text-shadow", "0px 0px 10px rgba(255, 255, 255, 1)")
        .text("");

      // Create a selection to hold the unique vistits text element
      let uniqueText = svg.append("text")
        .attr("x", width - 3*parseInt(offsets.pctLeft))
        .attr("y", parseInt(offsets.pctTop))
        .style("font-size", fonts.percent)
        .style("font-family", fonts.family)
        .style("fill", colors.percent)
        .style("text-anchor", "end")
        .style("pointer-events", "none")
        .style("text-shadow", "0px 0px 10px rgba(255, 255, 255, 1)")
        .text("");
    }

    // Fetch the data from the JSON file
    const jsonUrl = 'https://www.500foods.com/visitordata.json';
    fetchWebData(jsonUrl)
      .then(jsonData => {
        const processedData = processWebData(jsonData);

        const container = document.getElementById("divChart");
        const width = this.Param_Width;
        const height = this.Param_Height;
        const margin = { top: 5, right: 10, bottom: 160, left: 10 };
        const colors = {
          bg: "#f0f0f0",
          high: "#008000", // Color for repositories with high popularity
          med: "#006000", // Color for repositories with medium popularity
          low: "#004000", // Color for repositories with low popularity
          selected: "#40A040", // Color for selected repositories
          axisText: "#999999", // Color for axis labels and total unique visits
          axisLines: "#cccccc", // Color for axis lines
          label: "white", // Color for segment labels
          percent: "white" // color of percent indicator
        };
        const fonts = {
          family: "Cairo, sans-serif", // Font family for all text elements
          axis: "9px", // Font size for axis labels
          repo: "9px", // Font size for repository names (unused)
          label: "10px", // Font size for segment labels
          clip: "15px", // Minimum height threshold for displaying segment labels
          percent: "24px" // size of percent indicator
        };
        const offsets = {
          countOffset: "28px", // Vertical offset between date labels and total unique visits
          pctLeft: "10px",
          pctTop: "30px"
        };
        const rounding = "5px"; // Amount of rounding for bar segments
        const animTime = 1500; // Duration of the animation in milliseconds

        chartWebTraffic(
          processedData,
          container,
          width,
          height,
          margin,
          colors,
          fonts,
          offsets,
          rounding,
          animTime
        );
      })
      .catch(error => {
        console.error('Error processing data:', error);
      });
   } end;
   {$ENDIF}
   divChart.Visible := True;
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

    async function sleep(msecs) {
      return new Promise((resolve) =>setTimeout(resolve, msecs));
    }
    window.sleep = sleep;

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
//      pas.Unit1.Form1.UpdateChart();
    });
  end;


  // Parameters

  PauseChartUpdates := True;
  Highlight := '[none]';

  Param_Mode := 'UI';
  Param_Account := '';
  Param_GitHubToken := '';
  Param_Calendar := '';

  Param_Top := 0;
  Param_Left := 0;
  Param_Width := Form1.Width - 125;
  Param_Height := (Form1.Height div 2);

  Param_FontSize := 10;
  Param_Background := '#212529';
  Param_Scale := '1';


  // See if they're already in localStorage

  Param_Mode := TWebLocalStorage.GetValue('GAE.M');
  Param_Account := TWebLocalStorage.GetValue('GAE.A');
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
  if GetQueryParam('A') <> '' then Param_Account := GetQueryParam('A');
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
//    await(RefreshTableData);
//    await(UpdateTable);
    UpdateChart;
  end
  else if Param_Mode = 'WebTraffic' then
  begin
    document.documentElement.setAttribute('style','overflow:hidden;');
    WebEdit1.Visible := False;
    divTabulator.Visible := False;
    Form1.ElementClassName := '';
    divMain.ElementClassName := '';
    divChart.ElementClassName := 'overflow-hidden order-1';
//    await(RefreshTableData);
//    await(UpdateTable);
    UpdateWebTraffic;
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

function GetIntervalToNextHour: Cardinal;
var
  RightNow: TDateTime;
  NextHour: TDateTime;
begin
  RightNow := Now();
  NextHour := EncodeTime(HourOf(RightNow) + 1, 0, 0, 0);
  Result := Round((NextHour - RightNow) * MSecsPerDay);
end;

procedure TForm1.WebTimer1Timer(Sender: TObject);
begin
  WebTimer1.Enabled := False;
  WebTimer1.Interval := GetIntervalToNextHour;
  WebTimer1.Enabled := True;

  if Param_Mode = 'Calendar' then
  begin
    UpdateCalendar;
  end
  else if Param_Mode = 'Chart' then
  begin
    UpdateChart;
  end
  else if Param_Mode = 'WebTraffic' then
  begin
    UpdateWebTraffic;
  end;
end;

end.
