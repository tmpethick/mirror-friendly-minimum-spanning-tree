Get-ChildItem -Name -Exclude test04.uwg data | %{ java -jar .\Tamura.jar .\data\"$_" }
