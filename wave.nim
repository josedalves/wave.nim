
##-----------------------------------------------------------------------------
## wave.nim
##-----------------------------------------------------------------------------
##
##wave.nim provides a convenient API for reading and writing "wav" files
##

import streams
import macros

const RIFF_HEADER_SIZE* = 43 ##RIFF header size, for PCM type (non-PCM types have 2 additional bytes)
##Note that we are taking a lot of liberties here because "technically" RIFF
##can have any arbitrary size

const DEFAULT_CHANNELS* = 1
const DEFAULT_RATE* = 8000
const DEFAULT_WIDTH* = 1

type
  HeaderType* = enum
    ## Type of the header (and implicitly, data endianess).
    ## htRIFF and htRIFX have headers and htNONE has no header.
    ##
    ## The difference between RIFF and RIFX is that in RIFF files, data
    ## is little endiar, and RIFX is big endian
    htRIFF
    htRIFX
    htNONE

  StreamMode* = enum
    ## Stream mode. Readable, writable or both.
    smRead,
    smWrite,
    smReadWrite

  WaveParametersObj* = object
    ## Wave file parameters
    nsamples : uint32      # number of samples
    channels : uint16      # number of channels
    width : uint16         # number of bytes per sample
    rate : uint32          # sample rate
    endianness : Endianness # endianness
  WaveParameters* = ref WaveParametersObj

  WaveObj* = object
    ## A "Wave" object. Holds wave file parameters
    parameters : WaveParameters
    stream : FileStream
    header : HeaderType
    seekable : bool
    immediateUpdate : bool
    mode : StreamMode
  Wave* = ref WaveObj

  Sample* = object
    ## One sample. The size of "data" is channles * width
    data : seq[byte]

  WaveException = object of Exception
    error* : int

macro checkWritable(t : untyped) : untyped = 
  ## Pragma macro that checks if wav is writable before allowing writes
  var node =  parseStmt("""
if self.mode == smREAD:
  waveErr("Not writable")
""")
  insert(body(t), 0, node)
  result = t

macro checkOpen(t : untyped) : untyped = 
  ## Pragma macro that checks if stream is open
  var node =  parseStmt("""
if not self.isOpen():
  waveErr("Stream not open")
""")
  insert(body(t), 0, node)
  result = t

proc waveErr(s : string, error : int = 0) = 
  var e = new WaveException
  e.msg = s
  e.error = error
  raise e


# property getters and setters
proc channels*(self : Wave) : uint16 = 
  return self.parameters.channels

proc `channels=`*(self : Wave, c : uint16) = 
  if self.mode == smREAD:
    raise
  self.parameters.channels = c

proc width*(self : Wave) : uint16 = 
  return self.parameters.width

proc `width=`*(self : Wave, w : uint16) = 
  if self.mode == smREAD:
    raise
  self.parameters.width = w

proc rate*(self : Wave) : uint32 = 
  return self.parameters.rate

proc `rate=`*(self : Wave, r : uint32) = 
  if self.mode == smREAD:
    raise
  self.parameters.rate = r

proc nsamples*(self : Wave) : uint32 = 
  return self.parameters.nsamples

proc endianness*(self : Wave) : Endianness = 
  return self.parameters.endianness

proc `endianness=`*(self : Wave, e : Endianness) = 
  if self.mode == smREAD:
    raise
  self.parameters.endianness = e

proc newWave() : Wave = 
  result = new Wave
  result.parameters = new WaveParameters

proc parseHeader(self : Wave) = 
  ## (Re)parse the header. This will seek back to the beginning and parse the
  ## header.
  var buf : string
  var int32arr : array[4, uint8]
  var int16arr : array[2, uint8]
  var ht : HeaderType
  var chunksize : uint32
  var subchunk1size : uint32
  var datasize : uint32

  if self.stream == nil:
    raise


  # Helper to read an int from the header
  proc readUInt32() : uint32 {.inline.} = 
    if cpuEndian == bigEndian:
      for i in 3..0:
        int32arr[i] = cast[uint8](self.stream.readChar())
    else:
      for i in 0..3:
        int32arr[i] = cast[uint8](self.stream.readChar())
    return cast[uint32](int32arr)

  # Helper to read an int16 from the header
  proc readUInt16() : uint16 {.inline.} = 
    if cpuEndian == bigEndian:
      for i in 1..0:
        int16arr[i] = cast[uint8](self.stream.readChar())
    else:
      for i in 0..1:
        int16arr[i] = cast[uint8](self.stream.readChar())
    return cast[uint16](int16arr)

  self.header = htNONE
  self.parameters.channels = DEFAULT_CHANNELS
  self.parameters.width = DEFAULT_WIDTH
  self.parameters.rate = DEFAULT_RATE
  self.parameters.endianness = littleEndian

  self.stream.setPosition(0)

  # Check for RIFF string
  buf = self.stream.readStr(4)
  if buf == "RIFF":
    ht = htRIFF
  elif buf == "RIFX":
    ht = htRIFX
  else:
    # No header. Assume nothing. Return.
    return

  self.header = ht

  # chunk size
  chunksize = readUInt32()

  # WAVE
  if self.stream.readStr(4) != "WAVE":
    raise

  # 'fmt ', beginning of sub chunk 1
  if self.stream.readStr(4) != "fmt ":
    raise

  # sub chunk 1 size
  subchunk1size = readUInt32()

  if subchunk1size != 16:
    # always 16 for PCM. We don't know how to deal with anything else, so, die
    raise

  # Audio format. We only support (1).
  if readUInt16() != 1:
    raise

  # Channels
  self.parameters.channels = readUInt16()

  # Rate
  self.parameters.rate = readUInt32()

  # Byte rate. Redundant: this is equal to "rate * channels * width"
  discard readUInt32()

  # Block align. Redundant: this is equal to channels / width
  discard readUInt16()

  # Bits per sample. We store bytes per sample.
  self.parameters.width = readUInt16() div 8
  
  # Only available when type != PCM
  # discard readUInt16()

  # Data sub chunk (sub chunk 2) begins now
  if self.stream.readStr(4) != "data":
    # We screwed up somewhere
    raise

  # Size of data. channels * width * number of samples
  datasize = readUInt32()

  self.parameters.nsamples = datasize div (self.channels * self.width)

  # set seek position, just in case
  self.stream.setPosition(RIFF_HEADER_SIZE)

proc open*(self : Wave, file : string) = 
  ## Open a wav file for reading. The header is automatically parsed
  self.stream = newFileStream(file)
  parseHeader(self)

proc flush*(self : Wave) = 
  ## Flush wav file
  self.stream.flush()

proc close*(self : Wave) = 
  ## Close wav file
  self.flush()
  self.stream.close()
  self.stream = nil


proc isOpen*(self : Wave) : bool =
  ## Returns true if the file is open
  self.stream != nil

proc readFrames*(self : Wave, n : int) : seq[Sample] {.checkOpen.} = 
  ## Read n sample from file. File must already be open.
  if not self.isOpen():
    raise

  result = @[]

  for i in 1..n:
    var sample : Sample
    sample.data = @[]
    for byte in self.stream.readStr(cast[int](self.width * self.channels)):
      sample.data.add(cast[uint8](byte))
    result.add(sample)

proc readAll*(self : Wave) : seq[Sample] = 
  ## Read all samples, from current position to end.
  if not self.isOpen():
    raise

  result = @[]

  while not self.stream.atEnd():
    var sample : Sample
    sample.data = @[]
    for byte in self.stream.readStr(cast[int](self.width * self.channels)):
      sample.data.add(cast[uint8](byte))
    result.add(sample)

proc rewind*(self : Wave) = 
  ## Rewind strea,
  case self.header:
  of htNONE:
    self.stream.setPosition(0)
  else:
    self.stream.setPosition(RIFF_HEADER_SIZE)

proc writeFrames(self : Wave, data : string) {.checkWritable.} = 
  discard

proc writeFrames(self : Wave, samples : seq[Sample]) = 
  discard

var wav = newWave()
discard wav.readFrames(10)

wav.open("Front_Center.wav")
var frs = wav.readFrames(10000)
var al = wav.readAll()
wav.writeFrames("foo")
wav.close()

echo len(al)

