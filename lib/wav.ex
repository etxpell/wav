defmodule Wav do

  def split(fname, opts \\ [])
  def split(fname, opts) do
    {:ok, stat} = File.stat(fname)
    IO.puts "size: #{inspect stat.size}"
    print_wave_info(fname)

    chunk_spec = list_chunks(fname)
    IO.puts "chunks: #{inspect chunk_spec}"

    s = set_opts(opts, wave_info(fname))

    spec = data_chunk(chunk_spec)
    time("look_for_hits", fn() -> look_for_hits(fname, s, spec) end)
    
  end

  defp time(str, func) do
    {t, r} = :timer.tc(func)
    IO.puts "#{str}: #{inspect div(t, 1000)}"
    r
  end

  def show(fname, pos, len \\ 10) do
    s = set_opts([], wave_info(fname))
    {:ok, fd} = :file.open(fname, [:binary, :read])
    s = Map.put(s, :fd, fd)
    start = spec.pos + max((pos - len+1), 0)
    stop  = start+1+2*(len-1)
    :file.position(fd, start)
    show2(s, start..stop)
    :file.close(fd)
  end

  defp show2(s, []) do
    []
  end
  defp show2(s, [pos|rest]) do
    case read_block(s) do
      ints when is_list(ints) ->
        IO.puts " >> #{inspect pos} : #{inspect ints}"
        show2(s, rest)
    end
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
          reposition_file_to_trigger_flank(s, 12)
          pos = read_pos(s)
          sample_num = pos_to_sample_num(pos, s)
          orig_sample_num = pos_to_sample_num(orig_pos, s)
          tails = [] #trigger_tail(s) # 
          hit = %{hit_no: s.hit_count, pos: pos, sample_num: sample_num, tails: tails}
          save_hit_in_output_file(hit, s)
          pos_after = read_pos(s)
          IO.puts "pos: #{inspect pos} / #{inspect sample_num}, orig: #{inspect orig_pos} / #{inspect orig_sample_num}, after: #{inspect pos_after}"
          look_for_hits2(inc_hit_count(s), [hit | hits])
        else
          look_for_hits2(s, hits)
        end
      err ->
        IO.puts "fread: #{inspect err}"
        Enum.reverse(hits)
    end
  end

  defp save_hit_in_output_file(hit, s) do
    f = output_filename(hit, s)
    len = s.block * s.copy_sz
    body = fread(s.fd, len)
    body_len = byte_size(body)
    if len != body_len do IO.puts "lengths differ #{inspect len} #{inspect body_len}" end
    
    bin = <<
      "RIFF",
      body_len + 32 :: little-integer-size(32),
      "WAVE",
      s.fmt_chnk :: binary,
      "data",
      body_len :: little-integer-size(32),
      body :: binary 
      >>

      :ok = File.write(f, bin)
  end

  defp output_filename(hit, s) do
    File.mkdir_p!("out/" <> s.rootname)
    "out/" <> s.rootname <> "/" <> s.rootname <> "-" <> inspect(hit.hit_no) <> ".wav"
  end

  defp trigger_tail(s) do
    fskip(s.fd, s.copy_sz * s.block)
    []
  end

  defp inc_hit_count(s), do: Map.put(s, :hit_count, s.hit_count + 1)

  #   bucks = [Bucket.new(trigger_point: 500, level: :full),
  #            Bucket.new(trigger_point: 300, level: :full),
  #            Bucket.new(trigger_point: 150, level: :full)]
  #   bin = fread(s.fd, 44100 * s.block)
  #   walk_bin(bin, 0, s, bucks, [])
  # end

  # defp walk_bin(bin, 44100, _s, _bucks, tails), do: tails
  # defp walk_bin(bin, n, s, bucks, tails) do
  #   {sample, rest} = read_block_from_bin(s, bin)
  #   bucks = Enum.map(bucks, fn(b) -> Bucket.sample(b, sample) end)
  #   {empty, non_empty} = Enum.split(bucks, &Bucket.is_empty?/1)
  #   new_tails = Enum.map(empty, fn(b) -> %{id: Bucket.trigger_point(b), sample: n} end) ++ tails
  #   walk_bin(rest, n + 1, s, non_empty, new_tails)
  # end

  defp read_pos(s) do
    {:ok, pos} = :file.position(s.fd, :cur)
    pos
  end

  defp set_pos_rel(s, offset) do
    {:ok, pos} = :file.position(s.fd, {:cur, offset})
    pos
  end

  defp reposition_file_to_trigger_flank(_, 0) do
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

  defp read_block_from_bin(s, bin) do
    sz = s.sample_len
    if s.ch == 1 do
      << a :: binary-size(sz), rest :: binary >> = bin
      {abs(bin_to_int(a)), rest}
    else 
      << a :: binary-size(sz), b :: binary-size(sz), rest :: binary >> = bin
      {max(abs(bin_to_int(a)), abs(bin_to_int(b))), rest}
    end
  end

  defp pos_to_sample_num(p, s), do: div(p - 80, s.block) + 1

  defp set_opts(opts, s) do
    opts |> Enum.reduce(s, &set_opt/2)
  end

  defp set_opt({:trig, val}, s), do: Map.put(s, :trig, val)
  defp set_opt({:bucket_add, val}, s), do: Map.put(s, :bucket_add, val)
  defp set_opt({:bucket_leak, val}, s), do: Map.put(s, :bucket_leak, val)


  defp data_chunk([d = %{ck: "data"} | _]), do: d
  defp data_chunk([_ | spec]), do: data_chunk(spec)

  defp bin_to_int(b) do
    << a :: little-integer-size(24) >> = b
    b = << a :: big-integer-size(24) >>
    << a :: signed-integer-size(24) >> = b
    a
  end

  def list_chunks(fname) when is_binary(fname) do
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

  # defp file_head(s) do
  #   # fill in actual length later, when the split is done
  #   len = 0000
  #   << "RIFF",
  #       len :: little-integer-size(32),
  #       "WAVE",
  #       "fmt ",
  #         16 :: little-integer-size(32),
  #         s.type :: little-integer-size(16),
  #         s.ch :: little-integer-size(16),
  #         s.slice_rate :: little-integer-size(32),
  #         s.data_rate :: little-integer-size(32),
  #         s.block :: little-integer-size(16),
  #         s.depth :: little-integer-size(16),
  #       "data",
  #       len :: little-integer-size(32),
  #   >>
  # end

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

    data = if hdr != "fmt " do
              fread(fd, len)
              b = fread(fd, 8)
              <<  _ :: binary-size(4),
                  len :: little-integer-size(32)
              >> = b
              fread(fd, len)
            else
              fread(fd, len)
           end

    << type :: little-integer-size(16),
      ch :: little-integer-size(16),
      slice_rate :: little-integer-size(32),
      data_rate :: little-integer-size(32),
      block :: little-integer-size(16),
      depth :: little-integer-size(16) >> = data

      fmt_chnk = << "fmt ", len :: little-integer-size(32), data :: binary >>


    <<"data",
      len :: little-integer-size(32) >> = fread(fd, 8)

    %{fd: fd,
      tot_len: tot_len,
      data_len: len,
      fname: fname,
      rootname: rootname(fname),
      trig: -6_000, #Keyword.get(opts, :trig, 30_000),
      copy_sz: 44_100,
      is_triggered: false,
      trig_bucket: 0,
      bucket_add: 100,
      bucket_leak: 1,
      lookaround_size: 10,
      hit_count: 0,
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
      sample_len: div(depth, 8),
      fmt_chnk: fmt_chnk
    }

  end

  defp rootname(s), do: to_string(:filename.rootname(to_charlist(s)))

end

defmodule Bucket do
  @full_watermark 200
  
  defmodule B do
    defstruct [
      :trigger_point,
      level: 0,
      add_for_high: 75,
      sub_for_low: 1,
      full_watermark: @full_watermark
    ]
  end

  def new(opts) do
    level = case Keyword.get(opts, :level) do
              :full -> @full_watermark
              x when is_integer(x) -> x
              _ -> 0
            end
    %B{trigger_point: Keyword.get(opts, :trigger_point),
       level: level}
  end

  defp is_empty?(%B{level: lev}), do: lev == 0

  def trigger_point(b), do: b.trigger_point

  def sample(b, s) do
    new = if s >= b.trigger_point do
            min(b.level + b.add_for_high, @full_watermark)
          else
            max(b.level - b.sub_for_low, 0)
          end
    %B{b | level: new}
  end

end


