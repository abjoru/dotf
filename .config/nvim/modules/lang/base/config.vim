if DotF#api#has_python()
  " simple format of Json data
  command! DfFormatJson %!python -m json.tool
endif

au BufWritePost *.scala Neomake! sbt
