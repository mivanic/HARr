write_RESPSE = function(headerName,
                        arr,
                        description = NULL,
                        coefficient = NULL,
                        maxSize = 1e6) {

  message(sprintf('%s with maxsize %s', headerName, maxSize))

  # If no description is provided, use the header name
  if (is.null(description)) {
    description = headerName
  }

  if (is.null(coefficient)) {
    coefficient = headerName
  }

  # Number of dimensions of the object
  dimensions =  dim(arr)
  if (is.null(dimensions)) {
    dimensions = c(1)
  }

  # real number arrays must have exactly seven dimensions
  dimensions = c(dimensions, rep(1, 7 - length(dimensions)))

  numberOfDimensions = length(dimensions)


  r = list()

  r[[1]] = writeBin(paste0(headerName, paste(rep(' ',4-nchar(headerName)),collapse='')), raw())[1:4]


  r[[2]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Type
      writeBin('RESPSE', raw())[1:6],
      # Description
      writeBin(paste0(description, paste0(
        rep(' ', 70 - nchar(description)), collapse = ''
      )), raw())[1:70],
      # Number of dimensions
      writeBin(numberOfDimensions, raw(), size = 4),
      # Each dimension size
      unlist(Map(
        function(f)
          writeBin(as.integer(f), raw(), size = 4),
        dimensions
      ))
    )

  # Record 3 contains the number of defined dimensions, number of used dimensions, coefficient name and used set names
  # Each dimension name

  if (is.null(dimnames(arr))) {
    setNames = as.raw(c(0x00, 0x00, 0x00, 0x00))
  } else {
    setNames =  c(unname(unlist(Map(
      function(f)
        writeBin(paste0(f, paste0(
          rep(' ', 12 - nchar(f)), collapse = ''
        )), raw())[1:12], names(dimnames(arr))
    )))
    ,
    as.raw(c(rep(
      0x6b, length(dimnames(arr))
    ))),
    as.raw(c(rep(
      0x00, 4 + 4 * length(dimnames(arr))
    )))
    #, as.raw(c(rep(0x00, 7)))
    )

  }

  r[[3]] =
    c(
      #Empty four characters
      writeBin('    ', raw())[1:4],
      # Defined dimensions
      writeBin(as.integer(length(unique(
        names(dimnames(arr))
      ))), raw()),
      as.raw(c(0xff, 0xff, 0xff, 0xff)),
      # Used dimensions
      writeBin(as.integer(length(names(
        dimnames(arr)
      ))), raw()),
      # coefficient name
      writeBin(paste0(coefficient, paste0(
        rep(' ', 12 - nchar(coefficient)), collapse = ''
      )), raw())[1:12],
      as.raw(c(0xff, 0xff, 0xff, 0xff)),
      setNames
    )


  for (ud in unique(names(dimnames(arr)))) {
    ele = dimnames(arr)[[ud]]
    r[[length(r) + 1]] = c(
      writeBin('    ', raw())[1:4],
      writeBin(1L, raw(), size = 4),
      writeBin(as.integer(length(ele)), raw(), size = 4),
      writeBin(as.integer(length(ele)), raw(), size = 4),
      unname(unlist(Map(
        function(f)
        {f=substr(f,1,12)
        writeBin(paste0(f, paste0(
          rep(' ', 12 - nchar(f)), collapse = ''
        )), raw())[1:12]}, ele
      )))

    )
  }

  # Get locations and dimensions
  val = as.vector(arr)
  pos = 1:length(val)
  nonzeros = val!=0
  nzval = val[nonzeros]
  nzpos = pos[nonzeros]


  if(length(nzval)<=maxSize/2){
    maxSize = length(nzval)+length(nzpos)
    numberDataRecords = 1
  }else{
    numberDataRecords = ceiling(2*length(nzval) / maxSize)
  }
  r[[length(r) + 1]] = as.raw(c(writeBin('    ', raw())[1:4],
                                writeBin(c(
                                  as.integer(length(nzval)),
                                  4L,
                                  4L
                                ), raw()),
                                writeBin(paste0(rep(' ', 16 * 5), collapse = ''), raw())[1:(16*5)]
                                ))



  dataHeaders = Map(function(dr){
    fromElement = (dr - 1) * maxSize/2 + 1
    toElement = min(dr * maxSize/2, length(c(nzval)))


    positions = nzpos[fromElement:toElement]
    values =  nzval[fromElement:toElement]

    if(!(length(positions)==1 && is.na(positions))){
      return(list(
        c(
          writeBin('    ', raw())[1:4],
          #writeBin(as.integer(numberDataRecords - (dr) + 1), raw(), size = 4),
          writeBin(as.integer(numberDataRecords+1-dr), raw(), size = 4),
          writeBin(as.integer(length(nzval)), raw(), size = 4),
          writeBin(as.integer(length(values)), raw(), size = 4),
          writeBin(as.integer(positions), raw(), size = 4),
          writeBin(as.double(values), raw(), size = 4)
        )))
    } else {
      return(list(
        c(
          writeBin('    ', raw())[1:4],
          #writeBin(as.integer(numberDataRecords - (dr) + 1), raw(), size = 4),
          writeBin(as.integer(numberDataRecords+1-dr), raw(), size = 4),
          writeBin(as.integer(0), raw(), size = 4),
          writeBin(as.integer(0), raw(), size = 4)
        )))
    }


  },1:numberDataRecords)

  return(c(r, unlist(dataHeaders,recursive=FALSE)))

}

