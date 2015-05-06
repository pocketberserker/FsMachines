C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe FsMachines.sln /property:Configuration=Release /property:VisualStudioVersion=12.0 /target:rebuild

.\.nuget\nuget.exe pack .\FsMachines\FsMachines.fsproj -Symbols -Properties VisualStudioVersion=12.0

