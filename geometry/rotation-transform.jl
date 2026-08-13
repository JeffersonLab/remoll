using Printf
using XML
using NonlinearSolve

Rz(θ) = [[cos(θ) -sin(θ) 0]; [sin(θ) cos(θ) 0]; [0 0 1]]
Ry(θ) = [[cos(θ) 0  sin(θ)]; [0 1 0]; [-sin(θ) 0  cos(θ)]]
Rx(θ) = [[1 0 0]; [0 cos(θ)  -sin(θ)];  [0 sin(θ) cos(θ)]]

function f(u,p)
    Tm = Rz(-p[3])*Ry(-p[2])*Rx(-p[1])*Rz(u[3])*Ry(u[2])*Rx(u[1])
    [
        Tm[1,1] - 1, Tm[1,2] - 0,  Tm[1,3] - 0, 
        Tm[2,1] - 0, Tm[2,2] - 1,  Tm[2,3] - 0, 
        Tm[3,1] - 0, Tm[3,2] - 0,  Tm[3,3] - 1, 
    ]
end


function get_angles(p::Vector{Float64} =  [π/12.5, π/1.4789 , π/2.3])
    u0 = [0,0,0]
    prob = NonlinearProblem(f,u0,p)
    sol = solve(prob, NewtonRaphson())
    #sol = solve(prob)
    u = sol.u
    return u
end

torad(degree) = degree/180*π
todeg(radian) = radian*180/π
todeg(p::Vector{Float64}) = [todeg(p[1]), todeg(p[2]), todeg(p[3])]

function test()
    pp = [-1,2,0]
    p =  [π/2,0, π/2]
    u = get_angles(p);
    p′ = Rx(p[1])*(Ry(p[2])*(Rz(p[3])*pp))
    q′ = Rz(u[3])*(Ry(u[2])*(Rx(u[1])*pp))
    println(todeg(p))
    println(todeg(u))
    println(p′)
    println(q′)
    @assert p′ - p′ < [0.001,0.001,0.001]
end




function transform()
    filename = "/mnt/stg/sft/remoll/develop-remoll/geometry/detector/ThinQuartz/DetectorArray/DetectorArray.gdml"
    for (i,line) in enumerate(readlines(filename))
        sl = strip(line)
        if(startswith(sl,"<rotation"))
            spc=split(line,'<')[1]
            parsed = parse(Node,sl)[1]
            x,y,z = tryparse(Float64,parsed["x"]), tryparse(Float64,parsed["y"]), tryparse(Float64,parsed["z"])
            tang = get_angles([torad(x),torad(y), torad(z)])
            xtd,ytd,ztd  = todeg(tang[1]), todeg(tang[2]), todeg(tang[3]) 
            #println("$(i), $xtd $ytd $ztd")
            st = @sprintf """%ic %s<rotation unit="deg" name="%s" x="%.4f" y="%.4f" z="%.4f" />""" i spc parsed["name"] xtd ytd ztd
            #stt = @sprintf """<rotation unit="deg" name="%s" x="%.4f" y="%.4f" z="%.4f" />""" parsed["name"] xtd ytd ztd
            #println()
            #println(sl)
            println(st)
            #println(stt)
            #println("$(parsed["name"]) -> ($x, $y, $z) --> ($xtd, $ytd, $ztd)")
            #println()
        end
    end
end

#test()
transform()
