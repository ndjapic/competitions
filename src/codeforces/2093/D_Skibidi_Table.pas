program D_Skibidi_Table;
var
    ntc, tci: int16;
    n, e, m: int8;
    q, i, x, y: int32;
    d: int64;
    ch: char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
		readln(q);

        for i := 1 to q do begin

            read(ch);
            read(ch);

            case ch of

                '>': begin
                    readln(x, y);
                    dec(x);
                    dec(y);
                    d := 0;
                    for e := n-1 downto 0 do begin
                        if odd(x shr e) and odd(y shr e) then
                            inc(d, int64(1) shl (2*e))
                        else if odd(x shr e) and not odd(y shr e) then
                            inc(d, int64(2) shl (2*e))
                        else if not odd(x shr e) and odd(y shr e) then
                            inc(d, int64(3) shl (2*e));
                    end;
                    writeln(d+1);
                end;

                '-': begin
                    readln(d);
                    dec(d);
                    x := 0;
                    y := 0;
                    for e := n-1 downto 0 do begin
                        m := (d shr (2*e)) and 3;
                        if odd(m) then begin
                            inc(y, int32(1) shl e);
                            m := 4-m;
                        end;
                        inc(x, int32(m div 2) shl e);
                    end;
                    writeln(x+1, ' ', y+1);
                end;

            end;

        end;
 
    end;

end.
