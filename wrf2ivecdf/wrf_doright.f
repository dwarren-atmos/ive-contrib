      program wrf_doright

      USE NETCDF

      character(len=80) :: infile, outfile
      character(len=2), parameter :: path='./'

      integer :: fid_in, fid_out , rcode
      integer, parameter :: nvars=11 !nvars=59
      integer :: n, i, j, k
      real, parameter :: missing = -9999.0
      character(len=10) :: vars(nvars),varnam
      character(len=80) :: units,descrp

      character(len=3) :: memord
c      integer :: xmem,ymem,zmem

      character(len=1) :: stagger

      integer :: varid, dimid(4)
      integer :: nx,ny,nz,nt,nxp1,nyp1,nzp1

      integer, allocatable :: ix(:),iy(:),iz(:),it(:)
      integer, allocatable :: xmem(:),ymem(:),zmem(:)
      real, allocatable :: dat(:,:,:), time(:), znu(:), znw(:), hgt(:,:)

      real, allocatable :: pbtop(:,:), pbbot(:,:),
     &                     ptop(:,:), pbot(:,:), psfc(:,:)

      real :: ppp, zmin_w, zmin_m, zmax_w, zmax_m

      real :: p_top(1), xmin,xmax,ymin,ymax,zmin,zmax,dx,dy,dz,
     &        stagx, stagy

      data vars/
     &  'U','V','W','PH','PHB','P','PB','T','MU','MUB','PSFC'
     &/
c      data vars/
c     &, 'U','V','W','PH','PHB','P','PB','T','MU','MUB','PSFC'
c     &, 'SR','POTEVP','SNOPCX','SOILTB','Q2','T2','TH2','U10'
c     &, 'V10','QVAPOR','LANDMASK','XICE','SFROFF','UDROFF'
c     &, 'IVGTYP','ISLTYP','VEGFRA','GRDFLX','SNOW','SNOWH'
c     &, 'RHOSN','CANWAT','SST','H_DIABATIC','TSK','RTHCUTEN'
c     &, 'RAINC','RAINNC','SNOWNC','GRAUPELNC','RTHRATEN'
c     &, 'RTHRATLW','RTHRATSW','SWDOWN','GLW','OLR','ALBEDO'
c     &, 'RUBLTEN','RVBLTEN','RTHBLTEN','TMN','XLAND','UST'
c     &, 'PBLH','HFX','QFX','LH','SNOWC'
c     &/

      allocate(ix(nvars),iy(nvars),iz(nvars),it(nvars))
      allocate(xmem(nvars),ymem(nvars),zmem(nvars))

      infile='./data/wrfout_d01_t0300_nosw_rh000_run2.cdf'
      outfile='./data/wrf_doright.cdf'

      call open_file(infile , nf_nowrite, fid_in )
      call create_file(outfile, nf_write , fid_out)
      call define_mode(fid_out,stop_define)

      ! get variable dimensions
      nt = get_dimlen(fid_in, 'Time')
      nx = get_dimlen(fid_in, 'west_east')
      ny = get_dimlen(fid_in, 'south_north')
      nz = get_dimlen(fid_in, 'bottom_top')
      nxp1 = get_dimlen(fid_in, 'west_east_stag')
      nyp1 = get_dimlen(fid_in, 'south_north_stag')
      nzp1 = get_dimlen(fid_in, 'bottom_top_stag')


      rcode = nf_get_att_real(fid_in, nf_global, 'DX', dx)
      rcode = nf_get_att_real(fid_in, nf_global, 'DX', dy)

c      allocate(mub(nx,ny),mu(nx,ny))
c      mumin=1e5 ; mumax=0.0
c      call get_variable2d(fid_in, 'MUB', nx, ny, 1, mub)
c      do n=1,nt
c       call get_variable2d(fid_in, 'MU', nx, ny, n, mu)
c       do i=1,nx ; do j=1,ny
c         muloc = mub(i,j) + mu(i,j)
c         mumax = max(mumax,muloc)
c         mumin = max(mumin,muloc)
c       end do ; end do;
c      end do
c      deallocate(mub,mu)

c      do n=1,nt
c       do i=1,nx ; do j=1,ny
c       end do ; end do;
c      end do
      allocate(time(nt),znu(nz),znw(nzp1),hgt(nx,ny))

      ! define dimensions in new file
      call define_mode(fid_out,start_define)
      call define_dim(fid_out,'TIME',nt,timeid)
      call define_dim(fid_out,'one',1,oneid)
      call define_dim(fid_out,'west_east',nx,nxid)
      call define_dim(fid_out,'south_north',ny,nyid)
      call define_dim(fid_out,'bottom_top',nz,nzid)
      call define_dim(fid_out,'west_east_stag',nxp1,nxp1id)
      call define_dim(fid_out,'south_north_stag',nyp1,nyp1id)
      call define_dim(fid_out,'bottom_top_stag',nzp1,nzp1id)

      ! define a few variables
      rcode=nf_def_var(fid_out,'TIME',nf_float,1,timeid,varid)
        rcode=nf_put_att_text(fid_out,varid,'def',4,'Time')
        rcode=nf_put_att_text(fid_out,varid,'units',3,'min')
        rcode=nf_put_att_text(fid_out,varid,'display_units',2,'hr')

      rcode=nf_def_var(fid_out,'ZNU',nf_float,1,nzid,varid)
        rcode=nf_put_att_int(fid_out,varid,'no_button',nf_int,1,1)
        rcode=nf_put_att_text(fid_out,varid,'units',0,'')

      rcode=nf_def_var(fid_out,'ZNW',nf_float,1,nzp1id,varid)
        rcode=nf_put_att_int(fid_out,varid,'no_button',nf_int,1,1)
        rcode=nf_put_att_text(fid_out,varid,'units',0,'')

      dimid(1) = nxid ; dimid(2) = nyid
      rcode=nf_def_var(fid_out,'HGT',nf_float,2,dimid(1:2),varid)
        rcode=nf_put_att_int(fid_out,varid,'no_button',nf_int,1,1)
        rcode=nf_put_att_text(fid_out,varid,'units',6,'meters')

      rcode=nf_def_var(fid_out,'P_TOP',nf_float,1,oneid,varid)
        rcode=nf_put_att_int(fid_out,varid,'no_button',nf_int,1,1)
        rcode=nf_put_att_text(fid_out,varid,'units',2,'Pa')

      call define_mode(fid_out,stop_define)

      ! copy variables
      call get_variable_vec(fid_in, 'XTIME', nt, time)
      call write_variable_vec(fid_out, 'TIME', nt, time)

      call get_variable1d(fid_in, 'ZNU', nz, 1, znu)
      call write_variable_vec(fid_out, 'ZNU', nz, znu)

      call get_variable1d(fid_in, 'ZNW', nzp1, 1, znw)
      call write_variable_vec(fid_out, 'ZNW', nzp1, znw)

      call get_variable2d(fid_in, 'HGT', nx, ny, 1, hgt)
      call write_variable2d(fid_out, 'HGT', nx, ny, 1, hgt)

      call get_variable1d(fid_in, 'P_TOP', 1, 1, p_top)
      call write_variable_vec(fid_out, 'P_TOP', 1, p_top)
      deallocate(time,hgt)

!      zmax_w = p_top(1) ; zmax_m = 10e6  ! top of domain
!      zmin_w = p_top(1) ; zmin_m = p_top(1) ! bottom of domain

      allocate(pbtop(nx,ny), pbbot(nx,ny), 
     &         ptop(nx,ny), pbot(nx,ny), psfc(nx,ny))

      do n=1,nt
        call get_variable3d_local(fid_in,'PB',1,nx,1,ny,nz,nz,n,pbtop)
        call get_variable3d_local(fid_in,'PB',1,nx,1,ny, 1, 1,n,pbbot)
        call get_variable2d(fid_in, 'PSFC', nx, ny, n, psfc)
        call get_variable3d_local(fid_in,'P',1,nx,1,ny,nz,nz,n,ptop)
        call get_variable3d_local(fid_in,'P',1,nx,1,ny, 1, 1,n,pbot)
!        do i=1,nx ; do j=1,ny
!          ppp=psfc(i,j)            ; if(ppp.gt.zmin_w) zmin_w = ppp
!          ppp=pbot(i,j)+pbbot(i,j) ; if(ppp.gt.zmin_m) zmin_m = ppp
!          ppp=ptop(i,j)+pbtop(i,j) ; if(ppp.lt.zmax_m) zmax_m = ppp
!        end do ; end do
      end do
      deallocate(pbtop, pbbot, ptop, pbot, psfc)

      !zmin_w=znw(nzp1) ; zmax_w=znw(1); zmin_m=znu(nz) ; zmax_m=znu(1);
      zmin_w=znw(1) ; zmax_w=znw(nzp1); zmin_m=znu(1) ; zmax_m=znu(nz);
      dz = abs(zmin_w - zmax_w)/nz

      xmin=0. ; xmax=(nxp1-1)*dx ; ymin=0. ; ymax=(nyp1-1)*dy
      !zmin=znw(nzp1)*mumin + p_top(1) ; zmax=znw(1)*mumax + p_top(1)

      ! define ive friendly global attributes
      call define_mode(fid_out,start_define)
      call setive_gatts(fid_out,'x','x',xmin,xmax,dx,'m','km')
      call setive_gatts(fid_out,'y','y',ymin,ymax,dy,'m','km')
      call setive_gatts(fid_out,'z','eta',zmin_w,zmax_w,dz,'','')
      call setive_gatts(fid_out,'t','t',0,0,0,'min','hr')
      call define_mode(fid_out,stop_define)

      do k=1,nvars
        varnam=vars(k)
        write(6,'(2A)') 'DEFINING '//trim(varnam)

        stagger = get_varble_attr_char(fid_in, varnam, 'stagger')
        memord = get_varble_attr_char(fid_in, varnam, 'MemoryOrder')

        xmem(k)=index(memord,'X')
        ymem(k)=index(memord,'Y')
        zmem(k)=index(memord,'Z')

        select case(trim(stagger))
        case('X')
         dimid(1)=nxp1id ; dimid(2)=nyid ; dimid(3)=nzid 
         stagx=0.0 ; stagy=0.5
        case('Y')
         dimid(1)=nxid ; dimid(2)=nyp1id ; dimid(3)=nzid
         stagx=0.5 ; stagy=0.0
        case('Z')
         dimid(1)=nxid ; dimid(2)=nyid ; dimid(3)=nzp1id
         stagx=0.5 ; stagy=0.5
        case default
         dimid(1)=nxid ; dimid(2)=nyid ; dimid(3)=nzid
         stagx=0.5 ; stagy=0.5
        end select
        dimid(4)=timeid

        rcode = nf_inq_dimlen(fid_out, dimid(1), ix(k)) 
        rcode = nf_inq_dimlen(fid_out, dimid(2), iy(k)) 
        rcode = nf_inq_dimlen(fid_out, dimid(3), iz(k)) 
        rcode = nf_inq_dimlen(fid_out, dimid(4), it(k)) 

        if(xmem(k).eq.0) then
          dimid(1) = oneid ; ix(k) = 1 
        end if

        if(ymem(k).eq.0) then
          dimid(2) = oneid ; iy(k) = 1
        end if

        if(zmem(k).eq.0) then
          dimid(3) = oneid ; iz(k) = 1
        end if

        ! start define mode for new variables
        call define_mode(fid_out,start_define)

        rcode=nf_def_var(fid_out,trim(varnam),nf_float,4,dimid,varid)

        units = get_varble_attr_char(fid_in,trim(varnam),'units')
        descrp = get_varble_attr_char(fid_in,trim(varnam),'description')

        xmin= stagx*dx ; xmax=(real(ix(k)-1)+stagx)*dx
        ymin= stagy*dy ; ymax=(real(iy(k)-1)+stagy)*dy

        if(iz(k).eq.nz) then
          !zmin=znu(iz)*mumin + p_top(1) ; zmax=znu(1)*mumax + p_top(1)
          zmin=zmin_m ; zmax=zmax_m
        elseif(iz(k).eq.nzp1) then
          !zmin=znw(iz)*mumin + p_top(1) ; zmax=znw(1)*mumax + p_top(1)
          zmin=zmin_w ; zmax=zmax_w
        else
          zmin = 0.0 ; zmax = 0.0
        end if

        call setive_vatts(fid_out,varid,'x',xmin,xmax,units,descrp)
        call setive_vatts(fid_out,varid,'y',ymin,ymax,'','')
        call setive_vatts(fid_out,varid,'z',zmin,zmax,'','')

        call define_mode(fid_out,stop_define)
      end do

      do k=1,nvars
        varnam=vars(k)
        write(6,100) 'COPPYING '//trim(varnam)
        allocate(dat(ix(k),iy(k),iz(k)))

        do n=1,it(k)
          dat(:,:,:) = missing

          if(zmem(k).eq.0) then
            call get_variable2d(fid_in, trim(varnam), 
     &                          ix(k), iy(k), n, dat(:,:,1))
          elseif(ymem(k).eq.0) then
            call get_variable2d(fid_in, trim(varnam), 
     &                          ix(k), iz(k), n, dat(:,1,:))
          elseif(xmem(k).eq.0) then
            call get_variable2d(fid_in, trim(varnam), 
     &                          iy(k), iz(k), n, dat(1,:,:))
          else
            call get_variable3d(fid_in, trim(varnam), 
     &                          ix(k), iy(k), iz(k), n, dat)
          end if

          call write_variable3d(fid_out, trim(varnam), 
     &                          ix(k), iy(k), iz(k), n, dat)

          write(6,100) '.'
        end do

        deallocate(dat)
        print *,' '

      end do
      !Now lets copy the global attributes
      if(.not. copy_global_attr(fid_in, fid_out)) then
         write(6,*)'Got an error when copying global attributes'
      endif
      call close_file(fid_in)
      call close_file(fid_out)

      deallocate(znu,znw)

100   format(A,$)
      stop
      end program
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  setive_gatts (fid,dir,dirmin,dirmax,delta, domunits,disunits)
c
c  Set gloabal attributes (which help make the file self-describing)
c
c  Created: Alex Reinecke
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine setive_gatts(fid,dir,label,dirmin,dirmax,delta,
     &                           domunits,disunits)

      USE NETCDF

      integer, intent(in) :: fid
      character(len=*), intent(in) :: dir,label,domunits,disunits
      real, intent(in) :: dirmin,dirmax,delta

      character(len=80) :: attname

      integer :: rcode

      if(abs(dirmin-dirmax).gt.0) then
        write(attname,200) trim(dir),'_min'
        rcode=nf_put_att_real(fid,nf_global,
     &                        trim(attname),nf_float,1,dirmin)
c        if(rcode.ne.nf_noerr) call handle_err('x_min')

        write(attname,200) trim(dir),'_max'
        rcode=nf_put_att_real(fid,nf_global,
     &                        trim(attname),nf_float,1,dirmax)
c        if(rcode.ne.nf_noerr) call handle_err('x_max')
      end if

      if(delta.gt.0) then
        write(attname,200) trim(dir),'_delta'
        rcode=nf_put_att_real(fid,nf_global,
     &                        trim(attname),nf_float,1,delta)
c        if(rcode.ne.nf_noerr) call handle_err('x_delta')
      end if

      if(len(trim(domunits)).gt.0) then
        write(attname,200) trim(dir),'_units'
        rcode=nf_put_att_text(fid,nf_global,
     &                        trim(attname),len(trim(domunits)),
     &                        trim(domunits))
c        if(rcode.ne.nf_noerr) call handle_err('x_units')
      end if

      if(len(trim(disunits)).gt.0) then
        write(attname,200) trim(dir),'_display_units'
        rcode=nf_put_att_text(fid,nf_global,
     &                        trim(attname),len(trim(disunits)),
     &                        trim(disunits))
c        if(rcode.ne.nf_noerr) call handle_err('x_display_units')
      end if

      write(attname,200) trim(dir),'_label'
      rcode=nf_put_att_text(fid,nf_global,
     &                      trim(attname),len(trim(label)),label)
c      if(rcode.ne.nf_noerr) call handle_err('x_label')

200   format(A,A)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c setive_vatts
c
c  Set variable attributes (which help make the file self-describing)
c
c  Created: Alex Reinecke
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine setive_vatts(fid,vid,dir,dirmin,dirmax,domunits,descrp)

      USE NETCDF

      integer, intent(in) :: fid,vid
      character(len=*), intent(in) :: dir ,domunits, descrp
      real, intent(in) :: dirmin,dirmax
      !logical, intent(in) :: lbutton
      logical, parameter :: lbutton = .true.

      character(len=80) :: attname

      integer :: rcode

      if(len(trim(descrp)).gt.0) then
        rcode=nf_put_att_text(fid,vid,'description',
     &                        len(trim(descrp)),trim(descrp))
c        if(rcode.ne.nf_noerr) call handle_err('x_units')
      end if

      if(len(trim(domunits)).gt.0) then
        rcode=nf_put_att_text(fid,vid,'units',
     &                         len(trim(domunits)),trim(domunits))
c        if(rcode.ne.nf_noerr) call handle_err('x_units')
      end if

      if(.not.lbutton) then
        rcode=nf_put_att_int(fid,vid,'no_button',nf_int,1,1)
c        if(rcode.ne.nf_noerr) call handle_err('x_min')
      end if

      if(abs(dirmin-dirmax).gt.0) then
        write(attname,'(A,A)') trim(dir),'_min'
        rcode=nf_put_att_real(fid,vid,
     &                        trim(attname),nf_float,1,dirmin)
c        if(rcode.ne.nf_noerr) call handle_err('x_min')

        write(attname,'(A,A)') trim(dir),'_max'
        rcode=nf_put_att_real(fid,vid,
     &                        trim(attname),nf_float,1,dirmax)
c        if(rcode.ne.nf_noerr) call handle_err('x_max')
      end if

      end
