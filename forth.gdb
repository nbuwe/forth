# $Id$

break *(&breakpoint_code)

define pstack
  printf "\nParameter Stack:\n\n"
  printf "     TOS: %08x %11d\n", $r8, $r8
  set $psp = $r9
  while ($psp < &stack_bottom)
    printf "%08x: %08x %11d\n", $psp, \
    	*(unsigned int *)$psp, *(unsigned int *)$psp
    set $psp += 4
  end
end

define rstack
  printf "\nReturn Stack:\n\n"
  printf "      IP: %08x = ", $r11
  info symbol $r11
  set $rsp = $r10
  while ($rsp < &rstack_bottom)
    printf "%08x: %08x = ", $rsp, *(unsigned int *)$rsp
    info symbol *(unsigned int *)$rsp
    set $rsp += 4
  end
end

define state
  rstack
  pstack
end

display/i $pc
