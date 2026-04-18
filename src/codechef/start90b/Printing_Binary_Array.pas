program Printing_Binary_Array;
var
    notc: int32;
    n, i: int32;
    ai: int8;

begin
    readln(notc);
    repeat

        readln(n);
        for i := 0 to n-1 do begin
            read(ai);
            write(1-ai, ' ');
        end;
        readln;
        writeln;

        dec(notc);
    until notc = 0;
end.

