EPX plugin for HEX
==================

# Input events

    [{type,button},{id,a},{text,"Press"},{x,1},{y,1}]

    [{type,slider},{id,b},{x,10},{y,1},{width,100},{height,5}]

    [{type,button},{id,c},{image,"but.png"},{x,40},{y,40}]

# Output events

    [{type,rectangle},{id,r},{x,0},{y,50},{width,64},{height,64},
     {fill, solid}, {color, 16#ffff00ff}]

# Panel

  [{type,panel},{id,inout},{tabs,["output","input"]},
   {x,0},{y,0},{width,100},{height,60}],

  [{type,panel},{id,out},{panel,inout},{tabs,["1","2","3","4"]}]
  
  [{type,slider},{id,"output.1.delay"},{y,5},{x,1},{width,50},{height,10}]
  [{type,slider},{id,"output.1.rampup"},{y,20},{x,1},{width,50},{height,10}]
  [{type,slider},{id,"output.1.rampdown"},{y,35},{x,1},{width,50},{height,10}]
  [{type,slider},{id,"output.1.sustain"},{y,50},{x,1},{width,50},{height,10}]
