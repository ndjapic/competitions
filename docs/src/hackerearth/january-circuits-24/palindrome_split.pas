program palindrome_split;
{$H+}
var
    ntc: int8;
    n, i, ans: int16;
    s: string;
    ch: char;
    seen: array ['a' .. 'z'] of boolean;
 
begin
    readln(ntc);
    repeat
 
        for ch := 'a' to 'z' do seen[ch] := false;
 
        readln(s);
        n := length(s);
        ans := 0;
 
        for i := 1 to n do
            if seen[s[i]] then begin
                inc(ans);
                seen[s[i]] := false;
            end else
                seen[s[i]] := true;
 
        writeln(ans);
 
        dec(ntc);
    until ntc = 0;
end.
