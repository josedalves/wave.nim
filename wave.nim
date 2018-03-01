
import streams


const RIFF_HEADER_SIZE = 43

type
  HeaderType = enum
    htRIFF
    htRIFX
    htNONE

  WaveObj = object
    nsamples : uint32
    channels : uint16
    width : uint16
    rate : uint32
    endianess : Endianness
    stream : FileStream
    hasHeader : bool
    seekable : bool
  Wave = ref WaveObj

  Frame = object
    data : seq[byte]

## (Re)parse the header. This will seek back to the beginning and parse the
## header.
proc parseHeader(self : Wave) = 
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

  self.stream.setPosition(0)

  # Check for RIFF string
  buf = self.stream.readStr(4)
  if buf == "RIFF":
    ht = htRIFF
  elif buf == "RIFX":
    ht = htRIFX
  else:
    self.hasHeader = false
    self.channels = 0
    self.width = 0
    self.rate = 0
    self.endianess = littleEndian
    return
  # we assume RIFF for now


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
  self.channels = readUInt16()

  # Rate
  self.rate = readUInt32()

  # Byte rate. Redundant: this is equal to "rate * channels * width"
  discard readUInt32()

  # Block align. Redundant: this is equal to channels / width
  discard readUInt16()

  # Bits per sample. We store bytes per sample.
  self.width = readUInt16() div 8
  
  # Only available when type != PCM
  # discard readUInt16()

  # Data sub chunk (sub chunk 2) begins now
  if self.stream.readStr(4) != "data":
    # We screwed up somewhere
    raise

  # Size of data. channels * width * number of samples
  datasize = readUInt32()

  self.nsamples = datasize div (self.channels * self.width)

  # set seek position, just in case
  self.stream.setPosition(RIFF_HEADER_SIZE)

proc open(self : Wave, file : string) = 
  self.stream = newFileStream(file)
  parseHeader(self)

proc isOpen(self : Wave) : bool =  self.stream != nil

proc readFrames(self : Wave, n : int) : seq[Frame] = 
  if not self.isOpen():
    raise

  result = @[]

  for i in 1..n:
    var frame : Frame
    frame.data = @[]
    for byte in self.stream.readStr(cast[int](self.width * self.channels)):
      frame.data.add(cast[uint8](byte))
    result.add(frame)

proc rewind(self : Wave) = 
  discard

var wav = new Wave

wav.open("Front_Center.wav")
var frs = wav.readFrames(10000)


