#!/usr/bin/env ruby

restic_bin = '/usr/local/bin/restic'
restic_source = '/Users/sulami/Documents /Users/sulami/Pictures --exclude=".DS_Store"'
retention_policy = '--keep-daily 14 --keep-weekly 4 --keep-monthly 12 --keep-yearly 5'

ENV['B2_ACCOUNT_ID'] = `pass backblaze-b2/account-id`
ENV['B2_ACCOUNT_KEY'] = `pass backblaze-b2/account-key`
ENV['RESTIC_REPOSITORY'] = `pass restic/target`
ENV['RESTIC_PASSWORD'] = `pass restic/password`

case ARGV
when ['backup']
  puts `#{restic_bin} --host Robins-MacBook-Pro.local backup #{restic_source}`
when ['cleanup']
  puts `#{restic_bin} forget --host Robins-MacBook-Pro.local #{retention_policy} --prune`
when ['backup-and-cleanup']
  puts `#{restic_bin} --host Robins-MacBook-Pro.local backup #{restic_source}`
  puts `#{restic_bin} forget --host Robins-MacBook-Pro.local #{retention_policy} --prune`
else
  puts `#{restic_bin} #{ARGV.join(' ')}`
end
