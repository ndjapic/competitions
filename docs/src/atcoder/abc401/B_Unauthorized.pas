program B_Unauthorized;
{MODE DELPHI}
var
    n, i, c: int8;
    s: string;
    is_log_in: boolean;

begin
    readln(n);

    is_log_in := false;
    c := 0;
    for i := 1 to n do begin
        readln(s);
        case s[4] of
            'i': is_log_in := true;
            'o': is_log_in := false;
            'v': if not is_log_in then inc(c);
        end;
    end;

    writeln(c);
end.
