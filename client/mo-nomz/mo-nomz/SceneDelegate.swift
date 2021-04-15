//
//  SceneDelegate.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import UIKit

class SceneDelegate: UIResponder, UIWindowSceneDelegate {
    var window: UIWindow?
    
    func scene(_ scene: UIScene, openURLContexts URLContexts: Set<UIOpenURLContext>) {
        guard Persistence.loadState() != nil else { return }
        guard let context = URLContexts.first else { return }
        let completion = {
            DispatchQueue.main.async {
                self.launch()
            }
        }
        switch context.url.scheme {
        case "file":
            window?.rootViewController?.importFile(url: context.url, completion: completion)
            break;
        case "http", "https":
            window?.rootViewController?.importUrl(url: context.url, completion: completion)
            break;
        default: return
        }
    }
    
    func scene(_ scene: UIScene, continue userActivity: NSUserActivity) {
        print(userActivity.activityType)
    }
    
    private func launch() {
        let launchSb = UIStoryboard(name: "LaunchScreen", bundle: nil)
        let launchVc = launchSb.instantiateInitialViewController()
        let mainSb = UIStoryboard(name: "Main", bundle: nil)
        let mainVc = mainSb.instantiateInitialViewController()

        if Persistence.loadState() == nil {
            window?.rootViewController = launchVc
            let username = UUID().uuidString
            let completion = { (resp: CreateUserResponse) -> Void in
                Persistence.setState(state: State(username: username, userId: resp.userId))
                DispatchQueue.main.async {
                    self.window?.rootViewController = mainVc
                }
            }
            window?.rootViewController?.loadUser(username: username, completion: completion)
        } else {
            window?.rootViewController = mainVc
        }
    }

    func scene(_ scene: UIScene, willConnectTo session: UISceneSession, options connectionOptions: UIScene.ConnectionOptions) {
        // Use this method to optionally configure and attach the UIWindow `window` to the provided UIWindowScene `scene`.
        // If using a storyboard, the `window` property will automatically be initialized and attached to the scene.
        // This delegate does not imply the connecting scene or session are new (see `application:configurationForConnectingSceneSession` instead).
        guard let _ = (scene as? UIWindowScene) else { return }
        
        launch()
    }

    func sceneDidDisconnect(_ scene: UIScene) {
        // Called as the scene is being released by the system.
        // This occurs shortly after the scene enters the background, or when its session is discarded.
        // Release any resources associated with this scene that can be re-created the next time the scene connects.
        // The scene may re-connect later, as its session was not necessarily discarded (see `application:didDiscardSceneSessions` instead).
    }

    func sceneDidBecomeActive(_ scene: UIScene) {
        // Called when the scene has moved from an inactive state to an active state.
        // Use this method to restart any tasks that were paused (or not yet started) when the scene was inactive.
    }

    func sceneWillResignActive(_ scene: UIScene) {
        // Called when the scene will move from an active state to an inactive state.
        // This may occur due to temporary interruptions (ex. an incoming phone call).
    }

    func sceneWillEnterForeground(_ scene: UIScene) {
        // Called as the scene transitions from the background to the foreground.
        // Use this method to undo the changes made on entering the background.
    }

    func sceneDidEnterBackground(_ scene: UIScene) {
        // Called as the scene transitions from the foreground to the background.
        // Use this method to save data, release shared resources, and store enough scene-specific state information
        // to restore the scene back to its current state.
    }
}

