export namespace envcheck {
	
	export class EnvStatus {
	    RPortableInstalled: boolean;
	    TramaInstalled: boolean;
	    RscriptPath: string;
	    LibPath: string;
	
	    static createFrom(source: any = {}) {
	        return new EnvStatus(source);
	    }
	
	    constructor(source: any = {}) {
	        if ('string' === typeof source) source = JSON.parse(source);
	        this.RPortableInstalled = source["RPortableInstalled"];
	        this.TramaInstalled = source["TramaInstalled"];
	        this.RscriptPath = source["RscriptPath"];
	        this.LibPath = source["LibPath"];
	    }
	}

}

