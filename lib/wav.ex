defmodule Wav do
  @moduledoc """
  Documentation for Wav.
  """

  def split(fname, opts \\ [])
  def split(fname, opts) do
    {:ok, stat} = File.stat(fname)
    IO.puts "size: #{inspect stat.size}"
    #s = File.stream!(fname, [:read], 2048)
    print_wave_info(fname)
    # m = %{len: len,
    #       fname: fname,
    #       trig: Keyword.get(opts, :trig, 30_000),
    #       num_trigs: 0,
    #       base_out: base_out,
    #       out_num: 1,
    #       type: type,
    #       ch: ch,
    #       slice_rate: slice_rate,
    #       data_rate: data_rate,
    #       block: block,
    #       depth: depth
    #     }
s= wave_info(fname)
    base_out = rootname(fname)
    {:ok, outfd} = :file.open(base_out <> "-out-0.txt", [:binary, :write])
    
    :file.write(outfd, file_head(s))

    #copy_bytes(fd, outfd, s)
    #read_some_samples(bin, 6)
    #look_for_triggers(s, block, depth, len, trig)

    #write_some_samples(bin, 1600, outfd)

    :file.close(outfd)
  end

  defp swap(<< a,b,c >>), do: <<c,b,a>>

  defp file_head(s) do
    len = 0000
    << "RIFF",
        len :: little-integer-size(32),
        "WAVE",
        "fmt ",
          16 :: little-integer-size(32),
          s.type :: little-integer-size(16),
          s.ch :: little-integer-size(16),
          s.slice_rate :: little-integer-size(32),
          s.data_rate :: little-integer-size(32),
          s.block :: little-integer-size(16),
          s.depth :: little-integer-size(16)
    >>
  end
  defp fread(fd, len) do
    {:ok, bin} = :file.read(fd, len)
    bin
  end
  defp print_wave_info(fname) do
    s = wave_info(fname)
    IO.puts "========================================"
    IO.puts "file: #{inspect s.fname}, len: #{inspect s.tot_len}"
    IO.puts "   type: #{inspect s.type}, num: #{inspect s.ch}"
    IO.puts "   slice: #{inspect s.slice_rate}, data #{inspect s.data_rate}"
    IO.puts "   block #{inspect s.block} depth #{inspect s.depth}"
    IO.puts "   music len #{inspect s.data_len}"
  end
  
  defp wave_info(fname) do
    {:ok, fd} = :file.open(fname, [:binary, :read])
    << "RIFF", 
        tot_len :: little-integer-size(32),
        "WAVE" >> = fread(fd, 12)

  #  IO.puts "RIFF and WAVE, total len #{inspect tot_len}"

    b = fread(fd, 8)
    <<  hdr :: binary-size(4),
        len :: little-integer-size(32) 
    >> = b

    if hdr != "fmt " do
 #     IO.puts "discarding #{hdr}, #{inspect len} bytes"
      fread(fd, len)
      b = fread(fd, 8)
      <<  hdr :: binary-size(4),
          len :: little-integer-size(32) 
      >> = b
    end

#    IO.puts "hdr: #{inspect hdr}, len: #{inspect len}"
    data = fread(fd, len)
    << type :: little-integer-size(16),
      ch :: little-integer-size(16),
      slice_rate :: little-integer-size(32),
      data_rate :: little-integer-size(32),
      block :: little-integer-size(16),
      depth :: little-integer-size(16) >> = data


    <<hdr :: binary-size(4),
      len :: little-integer-size(32) >> = fread(fd, 8)
    :file.close(fd)

    %{tot_len: tot_len,
      data_len: len,
      fname: fname,
      #trig: Keyword.get(opts, :trig, 30_000),
      num_trigs: 0,
      #base_out: base_out,
      out_num: 1,
      type: type,
      ch: ch,
      slice_rate: slice_rate,
      data_rate: data_rate,
      block: block,
      depth: depth
    }

  end

  defp write_some_samples(bin, n, fd), do: write_some_samples(bin, 0, n, fd)
  
  defp write_some_samples(bin, n, n, fd) do
    IO.puts "#{inspect n} samples written"
  end

  defp write_some_samples(bin, n, _, fd) when byte_size(bin) < 6 do
    IO.puts "#{inspect n} samples written (end of binary)"
  end

  defp write_some_samples(bin, n, m, fd) do
    << a :: little-integer-size(24), b :: little-integer-size(24), bin :: binary >> = bin
    b33 = << a :: big-integer-size(24), b ::big-integer-size(24) >>
    << a :: signed-integer-size(24), b :: signed-integer-size(24)>> = b33
    s = [to_string(n), to_string(a), to_string(b)]
    s = Enum.join(s, ",")
    :file.write(fd, s <> "\n")
    write_some_samples(bin, n+1, m, fd)
  end

  defp rootname(s), do: to_string(:filename.rootname(to_charlist(s)))

  defp read_some_samples(bin, 0), do: bin
  defp read_some_samples(bin, n) do
    # << a :: signed-integer-size(24), b :: signed-integer-size(24), _ :: binary >> = bin
    # IO.puts "#{inspect n}: #{inspect a} #{inspect b} (as signed)"
    << a :: binary-size(3), b :: binary-size(3), _ :: binary >> = bin
    << x,y,z >> = a
#    IO.puts "  --- #{inspect x} #{inspect y} #{inspect z}"
    a = swap(a)
    b = swap(b)
    b33 = << a :: binary, b :: binary >>
    << a :: signed-integer-size(24), b :: signed-integer-size(24)>> = b33
    # IO.puts "swapped signed #{inspect n}: #{inspect a} #{inspect b} (as signed 1)"

    << a :: little-integer-size(24), b :: little-integer-size(24), bin :: binary >> = bin
    # IO.puts "little int #{inspect n}: #{inspect a} #{inspect b}"
    b33 = << a :: big-integer-size(24), b ::big-integer-size(24) >>
    << a :: signed-integer-size(24), b :: signed-integer-size(24)>> = b33
    # IO.puts "big signed int #{inspect n}: #{inspect a} #{inspect b} (as signed 2)"
    read_some_samples(bin, n-1)
  end

  defp read_header(fd) do
    {:ok, str} = :file.read(fd, 4)
    {:ok, binlen} = :file.read(fd, 4)
    len = len_to_int(binlen)
    {str, len}
  end

end



    # {:ok, fd} = :file.open(fname, [:read, :binary])

    # # {str, len} = read_header(fd)
    # # IO.puts "hdr: #{inspect str}, ck len: #{inspect len}"

    # # {str, len} = read_header(fd)
    # # IO.puts "hdr: #{inspect str}, ck len: #{inspect len}"

    # {:ok, bin} = :file.read(fd, 440000)
    # << "RIFF",
    #     len :: little-integer-size(32),
    #     "WAVE",
    #     bin :: binary >> = bin
    # IO.puts "RIFF and WAVE, len #{inspect len}"

    # <<  hdr :: binary-size(4),
    #     len :: little-integer-size(32),
    #     bin :: binary >> = bin
    # IO.puts "hdr: #{inspect hdr}, len: #{inspect len}"

    # << _ :: binary-size(len),
    #   bin :: binary >> = bin
    # IO.puts "ok data "

    # <<hdr :: binary-size(4),
    #   len :: little-integer-size(32),
    #   bin :: binary >> = bin
    # IO.puts "hdr: #{inspect hdr}, len: #{inspect len}"
    # << data :: binary-size(len),
    #   bin :: binary >> = bin
    # IO.puts "ok data "
    # << type :: little-integer-size(16),
    #   ch :: little-integer-size(16),
    #   slice_rate :: little-integer-size(32),
    #   data_rate :: little-integer-size(32),
    #   block :: little-integer-size(16),
    #   depth :: little-integer-size(16) >> = data

    # IO.puts "type: #{inspect type}, num: #{inspect num_ch}"
    # IO.puts "slice: #{inspect slice_rate}, data #{inspect data_rate}"
    # IO.puts "block #{inspect block} depth #{inspect depth}"

    # <<hdr :: binary-size(4),
    #   len :: little-integer-size(32),
    #   bin :: binary >> = bin
    # IO.puts "hdr: #{inspect hdr}, len: #{inspect len}"

    # :file.close(fd)

