defmodule Wav do

  def split(fname, opts \\ [])
  def split(fname, opts) do
    {:ok, stat} = File.stat(fname)
    IO.puts "size: #{inspect stat.size}"
    print_wave_info(fname)

    chunk_spec = list_chunks(fname)
    IO.puts "chunks: #{inspect chunk_spec}"

    s = set_opts(opts, wave_info(fname))

    base_out = rootname(fname)
    #{:ok, outfd} = :file.open("out/" <> base_out <> "-out-0.txt", [:binary, :write])

    #file_to_txt(fname, chunk_spec, s)
    {:ok, outfd} = :file.open("morr.txt", [:binary, :write])
    s = Map.put(s, :outfd, outfd)
    spec = data_chunk(chunk_spec)
    time("look_for_hits", fn() -> look_for_hits(fname, s, spec) end)
    

    #:file.write(outfd, file_head(s))

    #do_split(outfd, s, 0)

    :file.close(outfd)
  end

  defp time(str, func) do
    {t, r} = :timer.tc(func)
    IO.puts "#{str}: #{inspect div(t, 1000)}"
    r
  end

  defp look_for_hits(fname, s, spec) do
    {:ok, fd} = :file.open(fname, [:binary, :read])
    s = Map.put(s, :fd, fd)
    :file.position(fd, spec.pos)
    r = look_for_hits2(s, [])
    :file.close(fd)
    r
  end

  defp look_for_hits2(s, hits) do
    orig_pos = read_pos(s)
    case read_block(s) do
      ints when is_list(ints) ->
        # str = [pos | ints] |> Enum.map(&to_string/1) |> Enum.join(",")
        # :file.write(s.outfd, str <> "\n")
        if is_any_below_trigger(ints, s) do
          reposition_file_to_trigger_flank(s, 10)
          pos = read_pos(s)
          sample_num = pos_to_sample_num(pos, s)
          orig_sample_num = pos_to_sample_num(orig_pos, s)
          hit = %{pos: pos, orig_pos: orig_pos, sample_num: sample_num, 
                  orig_sample_num: orig_sample_num}
          fskip(s.fd, 44100 * s.block)
          pos_after = read_pos(s)
          IO.puts "pos: #{inspect pos} / #{inspect sample_num}, orig: #{inspect orig_pos} / #{inspect orig_sample_num}, after: #{inspect pos_after}"
          look_for_hits2(s, [hit | hits])
        else
          look_for_hits2(s, hits)
        end
      err ->
        IO.puts "fread: #{inspect err}"
        Enum.reverse(hits)
    end
  end

  defp read_pos(s) do
    {:ok, pos} = :file.position(s.fd, :cur)
    pos
  end

  defp set_pos_rel(s, offset) do
    {:ok, pos} = :file.position(s.fd, {:cur, offset})
    pos
  end

  defp reposition_file_to_trigger_flank(s, 0) do
    IO.puts "reposition_file_to_trigger_flank could not find flank"
  end
  defp reposition_file_to_trigger_flank(s, max) do
    set_pos_rel(s, - 2 * s.block)
    ints = read_block(s)
    if is_any_gt_zero(ints) do
      :ok
    else
      reposition_file_to_trigger_flank(s, max - 1)
    end
  end

  defp is_any_gt_zero([x | l]) do
    if x >= 0 do
      true
    else
      is_any_gt_zero(l)
    end
  end
  defp is_any_gt_zero(_), do: false

  defp is_any_below_trigger([x | l], s) do
    if x < s.trig do
      true
    else
      is_any_below_trigger(l, s)
    end
  end
  defp is_any_below_trigger(_, _), do: false

  defp read_block(s) do
    sz = s.sample_len
    case :file.read(s.fd, s.block) do
      {:ok, << a :: binary-size(sz), b :: binary-size(sz) >> } ->
        [bin_to_int(a), bin_to_int(b)]
      {:ok, << a :: binary-size(sz) >> } ->
        [bin_to_int(a)]
      err ->
        err
    end
  end

# pos: 499190 (sample: 83197, after: 763790
# pos: 1313894 (sample: 218981, after: 1578494
# pos: 2007950 (sample: 334657, after: 2272550
# pos: 2687996 (sample: 447998, after: 2952596
# pos: 3389048 (sample: 564840, after: 3653648
# pos: 4041686 (sample: 673613, after: 4306286
# pos: 4668056 (sample: 778008, after: 4932656
# pos: 5295554 (sample: 882591, after: 5560154
# pos: 5940560 (sample: 990092, after: 6205160
# pos: 6541964 (sample: 1090326, after: 6806564
# pos: 6993092 (sample: 1165514, after: 7257692

# pos: 499130 orig pos: 499190 (sample: 83188, after: 763730
# pos: 1313882 orig pos: 1313894 (sample: 218980, after: 1578482
# pos: 2007944 orig pos: 2007950 (sample: 334657, after: 2272544
# pos: 2687984 orig pos: 2687996 (sample: 447997, after: 2952584
# pos: 3389036 orig pos: 3389048 (sample: 564839, after: 3653636
# pos: 4041674 orig pos: 4041686 (sample: 673612, after: 4306274
# pos: 4668020 orig pos: 4668056 (sample: 778003, after: 4932620
# pos: 5295536 orig pos: 5295554 (sample: 882589, after: 5560136
# pos: 5940542 orig pos: 5940560 (sample: 990090, after: 6205142
# pos: 6541958 orig pos: 6541964 (sample: 1090326, after: 6806558
# pos: 6993086 orig pos: 6993092 (sample: 1165514, after: 7257686


  defp pos_to_sample_num(p, s), do: div(p - 80, s.block) + 1

  defp set_opts(opts, s) do
    opts |> Enum.reduce(s, &set_opt/2)
  end

  defp set_opt({:trig, val}, s), do: Map.put(s, :trig, val)
  defp set_opt({:bucket_add, val}, s), do: Map.put(s, :bucket_add, val)
  defp set_opt({:bucket_leak, val}, s), do: Map.put(s, :bucket_leak, val)


  defp file_to_txt(fname, chunk_spec, s) do
    base_out = rootname(fname)
    {:ok, outfd} = :file.open("out/" <> base_out <> ".txt", [:binary, :write])
    spec = data_chunk(chunk_spec)
    sample = sample_bytes(s)
    {:ok, fd} = :file.open(fname, [:binary, :read])
    IO.puts "data: #{inspect spec}, len: #{inspect sample * div(spec.len, sample)}"
    {:ok, bin} = :file.pread(fd, spec.pos, sample * div(spec.len, sample))
    :file.close(fd)
    copy_to_txt(bin, outfd, sample, s.ch)
    :file.close(outfd)
  end

  defp data_chunk([d = %{ck: "data"} | _]), do: d
  defp data_chunk([_ | spec]), do: data_chunk(spec)

  defp copy_to_txt(<< >>, outfd, sample, ch) do
    :ok
  end
  defp copy_to_txt(bin, fd, sample, ch) do
    bin2 = read_and_write_block(bin, fd, sample, ch)
    copy_to_txt(bin2, fd, sample, ch)
  end

  defp read_and_write_block(bin, fd, sample, ch) do
    << a :: binary-size(sample), bin2 :: binary >> = bin
    a = to_string(bin_to_int(a))
    if ch == 1 do
      :file.write(fd, a <> "\n")
      bin2
    else
      << b :: binary-size(sample), bin3 :: binary >> = bin2
      b = to_string(bin_to_int(b))
      :file.write(fd, a <> "," <> b <> "\n")
      bin3
    end
  end

  defp do_split(outfd, s, n) do
    if n >= s.data_len do
      # finalize file with correct lengths
      IO.puts "#{inspect n} bytes visited"
      IO.puts "  max: #{inspect s.max}, min: #{inspect s.min} "
    else
      b = fread(s.fd, 3)
      i = bin_to_int(b)
      s = update_trigger(s, i, n)
      s = update_max_min(s, i)
      do_split(outfd, s, n + 3)
    end
  end

  defp update_trigger(s, i, n) do
    if abs(i) > s.trig do
      handle_if_this_sample_triggered(s, i, n)
#      cond_debug(is_bucket_empty(s), "trig i: #{inspect i}, n: #{inspect n}, p: #{inspect p}")
      add_to_bucket(s)
    else
      leak_from_bucket(s)
    end
  end

  defp handle_if_this_sample_triggered(s, i, n) do
    if bucket_triggered(s) do
      p = :file.position(s.fd, :cur)
      IO.puts "trig i: #{inspect i}, n: #{inspect n}, p: #{inspect p}"
      look_back_some_samples(s)
      p = :file.position(s.fd, :cur)
      IO.puts "    file pos: #{inspect p}"
      s
    else
      s
    end
  end

  defp look_back_some_samples(s) do
    {:ok, p} = :file.position(s.fd, :cur)
    sample = sample_bytes(s)
    half_sz =  (div(s.lookaround_size, 2) * s.block)
    before =  if p - half_sz > 80 do
                p - half_sz
              else
                p - sample
              end
    sz =  if p - half_sz > 80 do
                2 * half_sz + sample
              else
                half_sz + sample
              end
    {ok, bin} = :file.pread(s.fd, before, sz)
    print_samples_from_bin(s, bin)
    :file.position(s.fd, p)
  end

  defp sample_bytes(s), do: div(s.depth, 8)

  defp print_samples_from_bin(s, bin) do
    case bin do
      << x :: binary-size(3), new_bin :: binary>> ->
        IO.puts "sample #{inspect bin_to_int(x)}"
        print_samples_from_bin(s, new_bin)
      << >> ->
        :ok
    end
  end

  defp bucket_triggered(s), do: is_bucket_empty(s)

  defp update_max_min(s = %{max: max}, i) when i > max, do: Map.put(s, :max, i)
  defp update_max_min(s = %{min: min}, i) when i < min, do: Map.put(s, :min, i)
  defp update_max_min(s, _), do: s
  
  defp cond_debug(true, str), do: IO.puts str
  defp cond_debug(_, _), do: :ok

  defp is_bucket_empty(s), do: s.trig_bucket == 0
  defp add_to_bucket(s), do: Map.put(s, :trig_bucket, s.bucket_add)
  defp leak_from_bucket(s), do: Map.put(s, :trig_bucket, max(s.trig_bucket-s.bucket_leak, 0))

  defp bin_to_int(b) do
    << a :: little-integer-size(24) >> = b
    b = << a :: big-integer-size(24) >>
    << a :: signed-integer-size(24) >> = b
    a
  end

  defp swap(<< a,b,c >>), do: <<c,b,a>>

  defp list_chunks(fname) when is_binary(fname) do
    {:ok, fd} = :file.open(fname, [:binary, :read])
    r = read_file_head(fd) ++ do_list_chunks(fd)
    :file.close(fd)
    r
  end

  defp read_file_head(fd) do
    << "RIFF",
    len :: little-integer-size(32),
    "WAVE" >> = fread(fd, 12)
    {:ok, p} = :file.position(fd, :cur)
    [%{ck: "RIFF WAVE", pos: p, len: len}]
  end

  defp do_list_chunks(fd) do
    case :file.read(fd, 8) do
      {:ok, bin} when byte_size(bin) == 8 ->
        <<  hdr :: binary-size(4),
            len :: little-integer-size(32) >> = bin
        {:ok, p} = :file.position(fd, :cur)
        fskip(fd, len)
        [%{ck: hdr, pos: p, len: len} | do_list_chunks(fd)]
      {:ok, << 0 >> } ->
        # Trailing zero
        []
      err ->
        IO.puts "file read: #{inspect err}"
        []
    end
  end

  defp file_head(s) do
    # fill in actual length later, when the split is done
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
          s.depth :: little-integer-size(16),
        "data",
        len :: little-integer-size(32),
    >>
  end

  defp fread(fd, len) do
    {:ok, bin} = :file.read(fd, len)
    bin
  end

  defp fskip(fd, len) do
    :file.position(fd, {:cur, len})
  end

  defp print_wave_info(fname) do
    s = wave_info(fname)
    :file.close(s.fd)
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

    b = fread(fd, 8)
    <<  hdr :: binary-size(4),
        len :: little-integer-size(32)
    >> = b

    if hdr != "fmt " do
      fread(fd, len)
      b = fread(fd, 8)
      <<  hdr :: binary-size(4),
          len :: little-integer-size(32)
      >> = b
    end

    data = fread(fd, len)
    << type :: little-integer-size(16),
      ch :: little-integer-size(16),
      slice_rate :: little-integer-size(32),
      data_rate :: little-integer-size(32),
      block :: little-integer-size(16),
      depth :: little-integer-size(16) >> = data


    <<hdr :: binary-size(4),
      len :: little-integer-size(32) >> = fread(fd, 8)

    %{fd: fd,
      tot_len: tot_len,
      data_len: len,
      fname: fname,
      trig: -6_000, #Keyword.get(opts, :trig, 30_000),
      is_triggered: false,
      trig_bucket: 0,
      bucket_add: 100,
      bucket_leak: 1,
      lookaround_size: 10,
      num_trigs: 0,
      max: 0,
      min: 0,
      #base_out: base_out,
      out_num: 1,
      type: type,
      ch: ch,
      slice_rate: slice_rate,
      data_rate: data_rate,
      block: block,
      depth: depth,
      sample_len: div(depth, 8)
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


