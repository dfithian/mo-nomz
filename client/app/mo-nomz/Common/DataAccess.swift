//
//  DataAccess.swift
//  mo-nomz
//
//  Created by Dan Fithian on 9/9/21.
//

import CoreData
import UIKit

class DataAccess: NSObject {
    public class var shared: DataAccess {
        struct Static {
            static let instance: DataAccess = DataAccess()
        }
        return Static.instance
    }
    
    lazy var persistentContainer: NSPersistentCloudKitContainer = {
        let container = NSPersistentCloudKitContainer(name: "Model")
        container.loadPersistentStores(completionHandler: { (storeDescription, error) in
            if let error = error {
                fatalError("Unresolved error, \((error as NSError).userInfo)")
            }
        })
        return container
    }()

    public lazy var managedObjectContext: NSManagedObjectContext = {
        return self.persistentContainer.viewContext
    }()
}
