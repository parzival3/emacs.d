# Run this script with admin right and after running the AnyConnect vpn
Get-NetAdapter | Where-Object {$_.InterfaceDescription -Match "Cisco AnyConnect"} | Set-NetIPInterface -InterfaceMetric 6000
$resolve_dns = (Get-NetAdapter | Where-Object InterfaceDescription -like "Cisco AnyConnect*" | Get-DnsClientServerAddress).ServerAddresses
$resolve_dns | ForEach-Object {"nameserver $_"} | wsl -u root tee -a /etc/resolv.conf