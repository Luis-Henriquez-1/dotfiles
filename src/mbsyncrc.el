IMAPAccount luishp
Host mail.luishp.xyz
User luis@luishp.xyz
PassCmd "pass email/luis@luishp.xyz | head -n 1"
SSLType IMAPS
CertificateFile /etc/ssl/certs/ca-certificates.crt
Port 993

IMAPStore luishp-remote
Account luishp

MaildirStore luishp-local
Subfolders Verbatim
Path ~/.mail/luis@luishp.xyz/
Inbox ~/.mail/luis@luishp.xyz/Inbox

Channel luishp
Master :luishp-remote:
Slave :luishp-local:
Create Slave
Sync All
Expunge Both
SyncState *
