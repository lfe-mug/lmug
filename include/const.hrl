
%% Error Messages

-define(UNK_CMD, "Unknown command.").
-define(NOREAD, "Could not read file; neither diretory, file, nor symlink.").
-define(MAX_FILES, "Total recursed files in given directory exceeds max-files setting.").
-define(MAX_FILE_SIZE, "File found with size exceeds max-file-size setting.").
-define(MAX_TOTAL_BYTES, "Total number of bytes exceeds max-total-bytes setting.").

-define(ERR_UNK_CMD, #{error => ?UNK_CMD}).
-define(ERR_NOREAD,  #{error => ?NOREAD}).
-define(ERR_MAX_FILES,  #{error => ?MAX_FILES}).
-define(ERR_MAX_FILE_SIZE,  #{error => ?MAX_FILE_SIZE}).
-define(ERR_MAX_TOTAL_BYTES,  #{error => ?MAX_TOTAL_BYTES}).
