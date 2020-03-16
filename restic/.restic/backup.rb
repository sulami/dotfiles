#!/usr/bin/env ruby

pass_bin = '/usr/local/bin/pass'
restic_bin = '/usr/local/bin/restic'
restic_source = '/Users/sulami/Documents /Users/sulami/Pictures --exclude=".DS_Store"'
retention_policy = '--keep-daily 14 --keep-weekly 4 --keep-monthly 12 --keep-yearly 5'

ENV['B2_ACCOUNT_ID'] = `#{pass_bin} backblaze-b2/account-id`.strip
ENV['B2_ACCOUNT_KEY'] = `#{pass_bin} backblaze-b2/account-key`.strip
ENV['RESTIC_REPOSITORY'] = `#{pass_bin} restic/target`.strip
ENV['RESTIC_PASSWORD'] = `#{pass_bin} restic/password`.strip

case ARGV
when ['backup']
  puts `#{restic_bin} backup --host Robins-MacBook-Pro.local #{restic_source}`
when ['cleanup']
  puts `#{restic_bin} forget --host Robins-MacBook-Pro.local #{retention_policy} --prune`
when ['backup-and-cleanup']
  puts `#{restic_bin} backup --host Robins-MacBook-Pro.local #{restic_source}`
  puts `#{restic_bin} forget --host Robins-MacBook-Pro.local #{retention_policy} --prune`
else
  puts `#{restic_bin} #{ARGV.join(' ')}`
end
