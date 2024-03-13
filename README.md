# TMS WEB Core GitHub API Example
This is a [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) example project that shows how to access and use data from the GitHub API.  It uses a supplied GitHub Personal Access token to retrieve repository data, specifically page views, in order to create a chart showing activity over the past two weeks - limited by the data provided through the GitHub API. Also, there is an option to display the GitHub Contribution Calendar by passing a GitHub username, which makes use of another project - [GitHub Calendar](https://github.com/Bloggify/github-calendar) - to do most of the heavy lifting.

This project was created as part of an ongoing blog series on the [TMS Software](https://www.timssoftware.com) website related to TMS WEB Core.  The specific blog post can be found here: [TMS WEB Core and More with Andrew: GitHub and GitHub Desktop](https://www.tmssoftware.com/site/blog.asp?post=1036). It was also referenced (and updated) in a related post about displaying custom data in an <iframe> in [Home Assistant](https://homeassistant.io), specifically the chart and the GitHub Contributions Canlendar: [TMS WEB Core and More with Andrew: Working with Home Assistant - Part 1: Introduction](https://www.tmssoftware.com/site/blog.asp?post=1044).
 
## Getting Started - Interactive Mode
This is a complete project that is intended to be entirely functional as-is. Simply load the project in any version of Delphi that supports TMS WEB Core.  This project was created using Delphi 10.3 and TMS WEB Core 2.05, for reference. Delphi 10+ should work fine, and any recent version of TMS WEB Core should work as well. Running the project will bring up the initial GitHub token request page.

![image](https://user-images.githubusercontent.com/41052272/226444080-34d1a15e-b186-4fb6-81ac-b1b1332faefb.png)

After entering a valid GitHub Personal Access Token with access to the page view information from a set of repositories, the page should update to show a list of those repositories with related information as well as chart showing the page view history available from GitHub.

![image](https://user-images.githubusercontent.com/41052272/226442536-cc137204-49d6-4d6f-bf68-0b1e61659495.png)

By default, all repositories are selected. Selecting or deselecting repositories listed in the table will add or remove them from the chart.  Hovering on a chart element will show the name of the repository and count.  The text labels are hidden if they are larger than the space available to draw them.  Clicking on a chart element will highlight all related elements from the same repository.  Click again to disable highlighting.

## Alternate Modes
Other modes are available by passing URL query parameters to the page.  These modes are used when you want to embed one of the charts in a <iframe> that is hosted on another website.  For example, this mechanism could be used to add a chart to the Home Assistant dashboard using a "Webpage card". The available parameters include the following.

- M - Mode (Chart or Calendar)
- G - Github API Personal Access Token
- C - Calendar Name 
- T - Top Margin 
- L - Left Margin
- W - Width
- H - Height
- B - Background
- F - Font size
- S - Scale (used with Calendar mode only at the moment)

## Chart Mode Example
To display just the chart, a GitHub API Personal Access Token must be included in the parameters.  This isn't stored anywhere else but is stored locally (localStorage) so that it doesn't have to be entered again if you subsequently access the application interactively. For example, to display the chart within Home Assistant, you might use a URL like the following.

```https://www.example.com/Project1.html?M=Chart&T=5&W=490&H=425&B=%231C1C1C&F=16&G=github_pat_use_your_own_token_here_oxw7ehI5kPeRgqMrXAdq3hm6AloW4XkojZYbWicB4M5HL7E67mcfAd4Do```

These parameters set the chart dimensions and font size to fit within a particular Home Assistant card arrangement and theme, so adjustments may be needed to fit into any particular Home Assistant dashboard.  Here's what it looks like when rendered there.

![image](https://github.com/500Foods/TMS-WEB-Core-GithubApiExample/assets/41052272/59fca86a-1286-420d-8449-85f772475291)

## Calendar Mode Example
To display just the GitHub Contributions Calendar, a similar set of parameters is required, including the GitHub username and GitHub API Personal Access Token. In previous versions of this project, a separate JavaScript library was used to retrieve this data, but that proved to be rather unreliable. Instead, it now uses GitHub's GraphQL interface to retrieve the data. This is then used to draw the calendar, along with CSS to control its formatting using primarily just CSS flex. Fancy! In any event, this is drawn differently than the regular chart, so it has been setup to use a CSS transform, scale() to adjust the size. For example, to fit it into a Home Assistant card, you might use something like this.

```https://www.example.com/Project1.html?M=Calendar&B=%231C1C1C&C=500Foods&S=0.25&G=github_pat_use_your_own_token_here_oxw7ehI5kPeRgqMrXAdq3hm6AloW4XkojZYbWicB4M5HL7E67mcfAd4Do```

Defaults are supplied for any missing parameters.  Here's what it looks like when rendered there. 

![image](https://github.com/500Foods/TMS-WEB-Core-GithubApiExample/assets/41052272/0d44f903-c974-4eac-92b9-a3fa4cae9fc6)

In both these examples, the background was set to something that matched the Home Assistant dashboard theme.  Any CSS color value works just as well here, including 'transparent' if you want the background of the hosting page to show through.

## Home Assistant Notes
There is also a GitHub Integration available for Home Assistant, which exposes several GitHub repository attributes as sensors that can be used elsewhere within Home Assistant. While there isn't sufficient data in these sensors to generate either the chart or the calendar as we've done in this project, the data may still be useful. Using a "multiple row entities" card (available from HACS), along with some "helpers" for the totals, the following was created using the same repositories as our other examples above.

![image](https://github.com/500Foods/TMS-WEB-Core-GithubApiExample/assets/41052272/be56bfd9-02fa-48a2-88b0-38e2920e7806)

## Additional Notes
The original version of this project used a more traditional Delphi approach to acquire data using TWeb-style components and methods. Over time this has been replaced with more direct JavaScript fetch() calls and a more streamlined approach to acquiring and formatting the data. This has led to an improvement in performance but a great deterioration in readability, as of course the JavaScript folks seem to prefer making things and terse and obscure as possible. 

## Key Dependencies
As with any modern web application, other JavaScript libraries/dependencies have been used in this project. Most of the time, this is handled via a CDN link (usually JSDelivr) in the Project.html file. In some cases, for performance or other reasons, they may be included directly.
- [TMS WEB Core](https://www.tmssoftware.com/site/tmswebcore.asp) - This is a TMS WEB Core project, after all
- [Bootstrap](https://getbootstrap.com/) - Version 5.3 support added with AdminLTE 4
- [Tabulator](https://www.tabulator.info) - Fantastic pure JavaScript web data tables
- [Luxon](https://moment.github.io/luxon/#/) - Handling date and time conversions
- [D3.js](https://d3js.org/) - Comprehensive JavaScript Charting Library

## Repository Information
[![Count Lines of Code](https://github.com/500Foods/TMS-WEB-Core-GithubApiExample/actions/workflows/main.yml/badge.svg)](https://github.com/500Foods/TMS-WEB-Core-GithubApiExample/actions/workflows/main.yml)
<!--CLOC-START -->
```
Last Updated at 2024-03-13 22:06:42 UTC
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Pascal                           2            194            448            724
Delphi Form                      1              0              0             83
CSS                              1              9              0             66
Markdown                         1             24              2             65
HTML                             2              7              6             34
YAML                             2              8             12             33
-------------------------------------------------------------------------------
SUM:                             9            242            468           1005
-------------------------------------------------------------------------------
```
<!--CLOC-END-->

## Sponsor / Donate / Support
If you find this work interesting, helpful, or valuable, or that it has saved you time, money, or both, please consider directly supporting these efforts financially via [GitHub Sponsors](https://github.com/sponsors/500Foods) or donating via [Buy Me a Pizza](https://www.buymeacoffee.com/andrewsimard500). Also, check out these other [GitHub Repositories](https://github.com/500Foods?tab=repositories&q=&sort=stargazers) that may interest you.

## More TMS WEB Core and TMS XData Content
If you're interested in other TMS WEB Core and TMS XData content, follow along on 𝕏 at [@WebCoreAndMore](https://x.com/WebCoreAndMore), join our 𝕏 [Web Core and More Community](https://twitter.com/i/communities/1683267402384183296), or check out the [TMS Software Blog](https://www.tmssoftware.com/site/blog.asp).
