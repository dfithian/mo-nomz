//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import CoreData
import UIKit
import Foundation

extension UIViewController {
    func loadUser(completion: ((CreateUserResponse) -> Void)?) {
        let spinner = startLoading()
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func loadExport(completion: (() -> Void)?) {
        let spinner = startLoading()
        let ctx = DataAccess.shared.managedObjectContext
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/export")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "GET"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "RecipeData")))
                    try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "GroceryItemData")))
                    try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "IngredientData")))
                    try ctx.save()
                    let output = try JSONDecoder().decode(ExportResponse.self, from: data!)
                    output.insert(ctx)
                    try ctx.save()
                    Persistence.setDidExport()
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func loadGroceries(completion: (([ReadableGroceryItemWithId]) -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let req = NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
        do {
            let groceries = try ctx.fetch(req).map({ $0.toReadableGroceryItemWithId() })
            completion?(groceries)
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func loadRecipes(completion: (([ReadableRecipeWithId]) -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let req = NSFetchRequest<RecipeData>(entityName: "RecipeData")
        do {
            let recipes = try ctx.fetch(req).map({ $0.toReadableRecipeWithId(ingredientsData: []) })
            completion?(recipes)
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func addBlob(content: String, completion: (() -> Void)?) {
        let spinner = startLoading()
        let ctx = DataAccess.shared.managedObjectContext
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            let onSuccess = {
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: data!)
                    output.insert(ctx)
                    try ctx.save()
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addBlob(content: String, name: String, link: String?, completion: (() -> Void)?) {
        let spinner = startLoading()
        let ctx = DataAccess.shared.managedObjectContext
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/blob")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseBlobRequest(content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Couldn't parse items. Please use one line per item, leading with the quantity.")
            }
            let onSuccess = {
                do {
                    let output = try JSONDecoder().decode(ParseBlobResponse.self, from: data!)
                    output.insert(ctx, name: name, link: link)
                    try ctx.save()
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addLink(link: String, active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        let ctx = DataAccess.shared.managedObjectContext
        guard let state = Persistence.loadState() else { return }
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v2/user/" + String(state.userId) + "/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ParseLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Unable to import recipe. Please enter ingredients manually.")
            }
            let onSuccess = {
                do {
                    let output = try JSONDecoder().decode(ParseLinkResponse.self, from: data!)
                    output.insert(ctx, link: link, active: active)
                    try ctx.save()
                    completion?()
                } catch {
                    self.defaultOnError(error)
                }
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: onSuccess, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func clearGroceryItems(completion: (() -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let recipeReq = NSFetchRequest<RecipeData>(entityName: "RecipeData")
        let ingredientReq = NSFetchRequest<IngredientData>(entityName: "IngredientData")
        let groceryReq = NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
        do {
            for recipe in try ctx.fetch(recipeReq) {
                recipe.active = false
            }
            for ingredient in try ctx.fetch(ingredientReq) {
                if ingredient.recipe_id == nil {
                    ctx.delete(ingredient)
                } else {
                    ingredient.grocery_id = nil
                }
            }
            for grocery in try ctx.fetch(groceryReq) {
                ctx.delete(grocery)
            }
            try ctx.save()
            completion?()
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func getRecipe(id: UUID, completion: ((ReadableRecipe) -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let recipeReq = NSFetchRequest<RecipeData>(entityName: "RecipeData")
        let ingredientReq = NSFetchRequest<IngredientData>(entityName: "IngredientData")
        recipeReq.predicate = NSPredicate(format: "id = %@", id as CVarArg)
        ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
        do {
            let ingredients = try ctx.fetch(ingredientReq)
            if let recipe = try ctx.fetch(recipeReq).first?.toReadableRecipe(ingredientsData: ingredients) {
                completion?(recipe)
            }
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func deleteGroceryItem(id: UUID, completion: (() -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let groceryReq = NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
        let ingredientReq = NSFetchRequest<IngredientData>(entityName: "IngredientData")
        groceryReq.predicate = NSPredicate(format: "id = %@", id as CVarArg)
        ingredientReq.predicate = NSPredicate(format: "grocery_id = %@", id as CVarArg)
        do {
            for grocery in try ctx.fetch(groceryReq) {
                ctx.delete(grocery)
            }
            for ingredient in try ctx.fetch(ingredientReq) {
                if ingredient.recipe_id != nil {
                    ingredient.grocery_id = nil
                } else {
                    ctx.delete(ingredient)
                }
            }
            try ctx.save()
            completion?()
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func deleteRecipe(id: UUID, completion: (() -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let recipeReq = NSFetchRequest<RecipeData>(entityName: "RecipeData")
        let ingredientReq = NSFetchRequest<IngredientData>(entityName: "IngredientData")
        let groceryReq = NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
        var groceryIds: [UUID] = []
        do {
            recipeReq.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            for recipe in try ctx.fetch(recipeReq) {
                ctx.delete(recipe)
            }
            ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
            for ingredient in try ctx.fetch(ingredientReq) {
                if let groceryId = ingredient.grocery_id {
                    groceryIds.append(groceryId)
                }
                ctx.delete(ingredient)
            }
            groceryReq.predicate = NSPredicate(format: "id in %@", (groceryIds) as CVarArg)
            for grocery in try ctx.fetch(groceryReq) {
                ctx.delete(grocery)
            }
            try ctx.save()
            completion?()
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func updateGroceryItem(id: UUID, grocery: ReadableGroceryItem, completion: (() -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let req = NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
        req.predicate = NSPredicate(format: "id = %@", id as CVarArg)
        do {
            if let fetched = try ctx.fetch(req).first {
                fetched.name = grocery.name
                fetched.quantity = Int32(grocery.quantity.toInt())
                fetched.unit = grocery.unit
                fetched.active = grocery.active
                fetched.ordering = Int32(grocery.order)
            }
            try ctx.save()
            completion?()
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func updateRecipe(id: UUID, recipe: ReadableRecipe, completion: (() -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let req = NSFetchRequest<RecipeData>(entityName: "RecipeData")
        req.predicate = NSPredicate(format: "id = %@", id as CVarArg)
        do {
            if let fetched = try ctx.fetch(req).first {
                if fetched.active != recipe.active {
                    let ingredientReq = NSFetchRequest<IngredientData>(entityName: "IngredientData")
                    ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
                    if recipe.active {
                        for ingredient in try ctx.fetch(ingredientReq) {
                            let grocery = ingredient.toReadableIngredientWithId().toGroceryItemWithId(active: recipe.active)
                            grocery.insert(ctx)
                            ingredient.grocery_id = grocery.id
                        }
                    } else {
                        var groceryIds: [UUID] = []
                        for ingredient in try ctx.fetch(ingredientReq) {
                            if let groceryId = ingredient.grocery_id {
                                groceryIds.append(groceryId)
                            }
                            ingredient.grocery_id = nil
                        }
                        let groceryReq = NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
                        groceryReq.predicate = NSPredicate(format: "id in %@", (groceryIds) as CVarArg)
                        for grocery in try ctx.fetch(groceryReq) {
                            ctx.delete(grocery)
                        }
                    }
                }
                fetched.active = recipe.active
                fetched.notes = recipe.notes
                fetched.rating = Int32(recipe.rating)
            }
            try ctx.save()
            completion?()
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
    
    func updateRecipeIngredients(id: UUID, active: Bool, deletes: [UUID], adds: [ReadableIngredientWithId], completion: (() -> Void)?) {
        let ctx = DataAccess.shared.managedObjectContext
        let req = NSFetchRequest<IngredientData>(entityName: "IngredientData")
        req.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
        do {
            let ingredients = try ctx.fetch(req)
            for ingredient in ingredients {
                if ingredient.id.map({ deletes.contains($0) }) ?? false {
                    ctx.delete(ingredient)
                }
            }
            for ingredient in adds {
                let grocery = ingredient.toGroceryItemWithId(active: active)
                grocery.insert(ctx)
                ingredient.insert(ctx, recipeId: id, groceryId: grocery.id)
            }
            try ctx.save()
            completion?()
        } catch let error as NSError {
            defaultOnError(error)
        }
    }
}
