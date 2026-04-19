# Problem: B_Not_Quite_Latin_Square.pas

```pascal
program B_Not_Quite_Latin_Square;
var
    ntc, tci: int8;
    i, j: int8;
    ch: char;
    seen: array ['A' .. 'C'] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        for i := 1 to 3 do begin

            for ch := 'A' to 'C' do seen[ch] := false;

            for j := 1 to 3 do begin
                read(ch);
                seen[ch] := true;
            end;
            readln;

            for ch := 'A' to 'C' do
                if not seen[ch] then writeln(ch);

        end;

    end;
end.

```
