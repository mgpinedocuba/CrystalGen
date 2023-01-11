module unit_cell_class

implicit none
! -----
! comentario:
! a - parámetro de red a [A]
! b - parámetro de red b [A]
! c - parámetro de red c [A]
! -----
	type,public::unit_cell
		real :: a=1.0
		real :: b=1.0
		real :: c=1.0
		real,dimension(3) :: lta=(/1.0,0.0,0.0/)
		real,dimension(3) :: ltb=(/0.0,1.0,0.0/)
		real,dimension(3) :: ltc=(/0.0,0.0,1.0/)
		integer :: na=1
		integer :: counter=0
		character(len=2),dimension(:),allocatable :: ba
		integer,dimension(:),allocatable :: bl
		real,dimension(:,:),allocatable :: bp
		integer :: sysn
		integer :: sysv
		character(len=2),dimension(:),allocatable :: sysa
		integer,dimension(:),allocatable :: sysl
		real,dimension(:,:),allocatable :: sysp
	contains
		procedure,public :: set_lpa => set_lpa_sb
		procedure,public :: set_lpb => set_lpb_sb
		procedure,public :: set_lpc => set_lpc_sb
		procedure,public :: set_lta => set_lta_sb
		procedure,public :: set_ltb => set_ltb_sb
		procedure,public :: set_ltc => set_ltc_sb
		procedure,public :: set_na => set_na_sb
		procedure,public :: add_atom => add_atom_sb
		procedure,public :: min_distance => min_distance_fn
		procedure,public :: ellipsoid => ellipsoid_sb
		procedure,public :: print_system => print_system_sb
		procedure,public :: max_neighbors => max_neighbors_fn
		procedure,public :: analysis => analysis_sb
	end type unit_cell
	private :: set_lpa_sb
	private :: set_lpb_sb
	private :: set_lpc_sb
	private :: set_lta_sb
	private :: set_ltb_sb
	private :: set_ltc_sb
	private :: set_na_sb
	private :: add_atom_sb
	private :: min_distance_fn
	private :: ellipsoid_sb
	private :: print_system_sb
	private :: max_neighbors_fn
	private :: analysis_sb
contains

	subroutine set_lpa_sb(this,a_par)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: a_par
		this%a=a_par
	end subroutine set_lpa_sb
	
	subroutine set_lpb_sb(this,b_par)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: b_par
		this%b=b_par
	end subroutine set_lpb_sb

	subroutine set_lpc_sb(this,c_par)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: c_par
		this%c=c_par
	end subroutine set_lpc_sb
	
	subroutine set_lta_sb(this,x_par,y_par,z_par)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: x_par,y_par,z_par
		this%lta(1)=x_par
		this%lta(2)=y_par
		this%lta(3)=z_par
	end subroutine set_lta_sb
	
	subroutine set_ltb_sb(this,x_par,y_par,z_par)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: x_par,y_par,z_par
		this%ltb(1)=x_par
		this%ltb(2)=y_par
		this%ltb(3)=z_par
	end subroutine set_ltb_sb
	
	subroutine set_ltc_sb(this,x_par,y_par,z_par)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: x_par,y_par,z_par
		this%ltc(1)=x_par
		this%ltc(2)=y_par
		this%ltc(3)=z_par
	end subroutine set_ltc_sb
	
	subroutine set_na_sb(this,n_par)
	implicit none
		class(unit_cell) :: this
		integer,intent(in) :: n_par
		integer :: stat
		character(len=99) :: msg
		this%na = n_par
		this%counter = 0
		if(allocated(this%ba))then
			deallocate(this%ba)
			allocate(this%ba(n_par),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		else
			allocate(this%ba(n_par),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		end if
		if(allocated(this%bl))then
			deallocate(this%bl)
			allocate(this%bl(n_par),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		else
			allocate(this%bl(n_par),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		end if
		if(allocated(this%bp))then
			deallocate(this%bp)
			allocate(this%bp(n_par,3),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		else
			allocate(this%bp(n_par,3),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		end if
	100 format(A)
	end subroutine set_na_sb
	
	subroutine add_atom_sb(this,ba_par,bl_par,bpa_par,bpb_par,bpc_par)
	implicit none
		class(unit_cell) :: this
		character(len=*),intent(in) :: ba_par
		integer,intent(in) :: bl_par
		real,intent(in) :: bpa_par,bpb_par,bpc_par
		if(this%counter==this%na)then
			write(*,100) "revisa el numero de átomos"
			stop
		end if
		this%counter = this%counter + 1
		this%ba(this%counter) = ba_par
		this%bl(this%counter) = bl_par
		this%bp(this%counter,1) = bpa_par
		this%bp(this%counter,2) = bpb_par
		this%bp(this%counter,3) = bpc_par
	100 format(A)
	end subroutine add_atom_sb
	
	subroutine cube(this,l,x,y,z,dx,dy,dz)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: l,x,y,z,dx,dy,dz
		real,dimension(3) :: dir
		
		dir = (/dx,dy,dz/)
		
	end subroutine cube
	
	subroutine cylinder(this,r,l,x,y,z,dx,dy,dz)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: r,l,x,y,z,dx,dy,dz
		
	end subroutine cylinder
	
	real function min_distance_fn(this)
	implicit none
		class(unit_cell) :: this
		real,dimension(:,:),allocatable :: rxyz
		real :: dist,dmin
		integer :: nx,ny,nz,na,ntot,l,i,j
		integer :: stat
		character(len=99) :: msg
		ntot = 27 * this%na
		allocate(rxyz(ntot,3),stat=stat,errmsg=msg)
		if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		l=0
		do nx=-1,1
			do ny=-1,1
				do nz=-1,1
					do na=1,this%na
						l=l+1
						rxyz(l,:) = nx*this%lta*this%a &
											+ ny*this%ltb*this%b &
											+ nz*this%ltc*this%c &
											+ this%lta*this%a*this%bp(na,1) &
											+ this%ltb*this%b*this%bp(na,2) &
											+ this%ltc*this%c*this%bp(na,3)
					end do
				end do
			end do
		end do
		dmin = 10000.0
		do i=1,ntot
			do j=1,ntot
				if(i.ne.j)then
					dist = norm2(rxyz(i,:)-rxyz(j,:))
					if(dist<dmin)then
						dmin=dist
					end if
				end if
			end do
		end do
		deallocate(rxyz)
		min_distance_fn = dmin
	100 format(A)
	end function min_distance_fn
	
	subroutine ellipsoid_sb(this,da,db,x,y,z,dx,dy,dz)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: da,db,x,y,z,dx,dy,dz
		real :: minxyz,diameter(2),maxr,rxyz(3),dir(3)
		integer :: nrep,i,j,k,a,l,numatom
		real :: proypar,proyper
		integer :: stat
		character(len=99) :: msg

		dir = (/dx,dy,dz/)
		dir = dir/norm2(dir)
		minxyz = this%min_distance()
		diameter = (/da,db/)
		maxr = maxval(diameter)
		nrep = int(maxr*10.0/minxyz)
		l=0
		do i=-nrep,nrep
			do j=-nrep,nrep
				do k=-nrep,nrep
					do a=1,this%na
						rxyz = i*this%lta*this%a &
								 + j*this%ltb*this%b &
								 + k*this%ltc*this%c &
								 + this%lta*this%a*this%bp(a,1) &
								 + this%ltb*this%b*this%bp(a,2) &
								 + this%ltc*this%c*this%bp(a,3)
						proypar = dot_product(dir,rxyz)
						proyper = norm2(rxyz-proypar*dir)
						if((proypar/(db*5.0))**2+(proyper/(da*5.0))**2<1.0)then
							l=l+1
						end if
					end do
				end do
			end do
		end do
		numatom = l
		this%sysn = numatom
		if(allocated(this%sysa))then
			deallocate(this%sysa)
			allocate(this%sysa(numatom),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		else
			allocate(this%sysa(numatom),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		end if
		if(allocated(this%sysl))then
			deallocate(this%sysl)
			allocate(this%sysl(numatom),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		else
			allocate(this%sysl(numatom),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		end if
		if(allocated(this%sysp))then
			deallocate(this%sysp)
			allocate(this%sysp(numatom,3),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		else
			allocate(this%sysp(numatom,3),stat=stat,errmsg=msg)
			if(stat/=0)then ; write(*,100) trim(msg) ; stop ; end if
		end if
		l=0
		do i=-nrep,nrep
			do j=-nrep,nrep
				do k=-nrep,nrep
					do a=1,this%na
						rxyz = i*this%lta*this%a &
								 + j*this%ltb*this%b &
								 + k*this%ltc*this%c &
								 + this%lta*this%a*this%bp(a,1) &
								 + this%ltb*this%b*this%bp(a,2) &
								 + this%ltc*this%c*this%bp(a,3)
						proypar = dot_product(dir,rxyz)
						proyper = norm2(rxyz-proypar*dir)
						if((proypar/(db*5.0))**2+(proyper/(da*5.0))**2<1.0)then
							l=l+1
							this%sysa(l) = this%ba(a)
							this%sysl(l) = this%bl(a)
							this%sysp(l,:) = rxyz
						end if
					end do
				end do
			end do
		end do
		close(50)
	100 format(A)
	101 format(A,I5,3f15.5)
	102 format(I6)
	end subroutine ellipsoid_sb
	
	subroutine print_system_sb(this,fname)
	implicit none
		class(unit_cell) :: this
		character(len=*) fname
		integer :: i
		open(unit=50,file=trim(fname),action="write")
			write(50,102) this%sysn
			write(50,100) " "
			do i=1,this%sysn
				write(50,*) this%sysa(i),this%sysl(i),this%sysp(i,:)
			end do
		close(50)
	100 format(A)
	101 format(A,I5,3f15.5)
	102 format(I6)
	end subroutine print_system_sb
	
	integer function max_neighbors_fn(this,rcut)
	implicit none
		class(unit_cell) :: this
		real,intent(in) :: rcut
		integer :: nmax,l
		integer :: i,j
		real :: dist
		nmax = 0
		do i=1,this%sysn
			l=0
			do j=1,this%sysn
				if(i.ne.j)then
					dist = norm2(this%sysp(j,:)-this%sysp(i,:))
					if(dist<rcut*1.001)then
						l=l+1
					end if
				end if
			end do
			if(nmax<l)then
				nmax = l
			end if
		end do
		max_neighbors_fn = nmax
	end function max_neighbors_fn
	
	subroutine analysis_sb(this)
	implicit none
		class(unit_cell) :: this
		integer :: nsurf,nbulk,nmax
		integer :: i,j,l
		real :: dmin,dist
		dmin = this%min_distance()
		nmax = this%max_neighbors(dmin)
		nbulk = 0
		nsurf = 0
		do i=1,this%sysn
			l=0
			do j=1,this%sysn
				if(i.ne.j)then
					dist = norm2(this%sysp(j,:)-this%sysp(i,:))
					if(dist<dmin*1.001)then
						l=l+1
					end if
				end if
			end do
			if(nmax==l)then
				nbulk = nbulk + 1
			else
				nsurf = nsurf + 1 
			end if
		end do
		write(*,*) "---------"
		write(*,*) "  data   "
		write(*,*) "---------"
		write(*,*) "surface: ",nsurf
		write(*,*) "bulk   : ",nbulk
		write(*,*) "total  : ",this%sysn
	end subroutine analysis_sb
	
end module unit_cell_class










